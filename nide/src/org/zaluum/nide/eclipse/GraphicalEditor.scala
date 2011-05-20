package org.zaluum.nide.eclipse

import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.ui.PlatformUI
import org.eclipse.jdt.core.IType
import org.eclipse.ui.internal.part.SelectionProviderAdapter
import org.zaluum.nide.Subject
import org.zaluum.nide.Observer
import org.zaluum.nide.zge.GuiViewer
import org.zaluum.nide.compiler.Parser
import org.zaluum.nide.zge.TreeViewer
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.Serializer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Location
import org.zaluum.nide.protobuf.BoxFileProtos
import org.eclipse.swt.events.DisposeEvent
import org.eclipse.ui.contexts.IContextService
import org.eclipse.swt.events.ShellEvent
import org.eclipse.swt.events.ShellAdapter
import org.eclipse.swt.events.ShellListener
import org.eclipse.swt.events.DisposeListener
import org.eclipse.ui.handlers.HandlerUtil
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.commands.ExecutionEvent
import java.io.ByteArrayInputStream
import org.eclipse.core.resources.{ IFile, IMarker }
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{ Composite, Shell }
import org.eclipse.ui.{ IEditorSite, IEditorInput, IEditorPart }
import org.eclipse.ui.ide.IGotoMarker
import org.eclipse.ui.part.{ EditorPart, FileEditorInput }
import org.zaluum.nide.zge.{ Viewer, Controller }
import org.eclipse.jdt.core.JavaCore
import org.zaluum.nide.eclipse.integration.model.ZaluumDomCompilationUnit
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnit
import org.eclipse.jdt.core.dom.ASTParser
import org.eclipse.jdt.core.dom.AST

class GraphicalEditor extends EditorPart with IGotoMarker {

  var viewer: TreeViewer = _
  var shell: Option[Shell] = None
  def controller = viewer.controller

  def doSave(monitor: IProgressMonitor) {
    controller.cu.commitWorkingCopy(true,monitor)
    controller.markSaved()
    firePropertyChange(IEditorPart.PROP_DIRTY)
  }

  def doSaveAs() {}

  def init(site: IEditorSite, input: IEditorInput) {
    setSite(site)
    setInput(input)
    setPartName(inputFile.getName)
    val contextService = getSite.getService(classOf[IContextService]).asInstanceOf[IContextService]
    contextService.activateContext("org.zaluum.nide.context")
  }

  def isDirty(): Boolean = { controller.isDirty }

  def isSaveAsAllowed(): Boolean = { false }

  def inputFile = getEditorInput.asInstanceOf[FileEditorInput].getFile
  def project = inputFile.getProject
  def zproject = ZaluumModelMananger.getOrCreate(project)
  def input = inputFile.getContents(true)
  def createPartControl(parent: Composite) {
    val zp = zproject.get
    val cu = JavaCore.createCompilationUnitFrom(inputFile)
    val controller = new Controller(cu)
    controller.addListener(fireDirty)
    viewer = new TreeViewer(parent, controller, zp,this)
    controller.registerViewer(viewer)
    getEditorSite().setSelectionProvider(selectionProvider);
    // TODO reopen
  }
  def adaptItem(i: IType): StructuredSelection = {
    val El = classOf[org.eclipse.jdt.core.IJavaElement]
    val adaptable = new org.eclipse.core.runtime.IAdaptable() {
      def getAdapter(cl: Class[_]) = {
        cl match {
          case El ⇒ i
          case _ ⇒ null
        }
      }
    }
    new StructuredSelection(adaptable)
  }
  def setSelection(i: IType) { selectionProvider.setSelection(adaptItem(i)) }
  private lazy val selectionProvider = new SelectionProvider()
  val fireDirty: () ⇒ Unit = () ⇒ firePropertyChange(IEditorPart.PROP_DIRTY)
  def setFocus() { viewer.canvas.setFocus }
  def openGUI() {
    if (!shell.isDefined) {
      val newshell = new Shell(getSite.getShell, SWT.MODELESS | SWT.CLOSE | SWT.RESIZE)
      newshell.setLayout(new FillLayout)
      newshell.setText(getTitle + " GUI");
      val guiViewer = new GuiViewer(newshell, controller, zproject.get)
      controller.registerViewer(guiViewer)
      newshell.layout()
      newshell.open()

      newshell.addDisposeListener(new DisposeListener() {
        override def widgetDisposed(e: DisposeEvent) {
          guiViewer.dispose()
          controller.unregisterViewer(guiViewer);
          shell = None
        }
      });
      shell = Some(newshell)
    }
  }
  override def dispose() {
    controller.removeListener(fireDirty)
    controller.dispose()
    viewer.dispose()
    shell foreach { s ⇒ if (!s.isDisposed) s.dispose }
  }
  override def gotoMarker(marker: IMarker) {
    val str = marker.getAttribute("BLAME").asInstanceOf[String]
    viewer.gotoMarker(Location.parse(str))
    setFocus
  }
  override def getAdapter(cl: Class[_]) = {
    if (cl == classOf[IGotoMarker]) this
    else super.getAdapter(cl)
  }
}

class OpenGUIHandler extends AbstractHandler {
  override def execute(event: ExecutionEvent) = {
    Option(HandlerUtil.getActiveEditor(event)) match {
      case Some(g: GraphicalEditor) ⇒ g.openGUI()
      case _ ⇒
    }
    null
  }
}