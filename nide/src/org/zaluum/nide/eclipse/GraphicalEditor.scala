package org.zaluum.nide.eclipse

import org.eclipse.core.commands.{AbstractHandler, ExecutionEvent}
import org.eclipse.core.resources.{IFile, IMarker}
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.swt.events.{DisposeEvent, DisposeListener}
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{Composite, Shell}
import org.eclipse.swt.SWT
import org.eclipse.ui.contexts.IContextService
import org.eclipse.ui.handlers.HandlerUtil
import org.eclipse.ui.ide.IGotoMarker
import org.eclipse.ui.part.EditorPart
import org.eclipse.ui.{IEditorInput, IEditorPart, IEditorSite}
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnit
import org.zaluum.nide.zge.{Controller, GuiViewer, TreeViewer, Viewer}
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.JavaCore

class GraphicalEditor extends BaseEditor with IGotoMarker {

  var viewer: TreeViewer = _
  var shell: Option[Shell] = None
  def controller = viewer.controller

  def doSave(monitor: IProgressMonitor) {
    controller.cu.commitWorkingCopy(true, monitor)
    controller.markSaved()
    firePropertyChange(IEditorPart.PROP_DIRTY)
  }

  def init(site: IEditorSite, input: IEditorInput) {
    setSite(site)
    setInput(input)
    setPartName(inputFile.getName)
    val contextService = getSite.getService(classOf[IContextService]).asInstanceOf[IContextService]
    contextService.activateContext("org.zaluum.nide.context")
  }

  def isDirty(): Boolean = { controller.isDirty }

  def createPartControl(parent: Composite) {
    val cu = JavaCore.createCompilationUnitFrom(inputFile)
    val zProject = new ZaluumProject(jproject)
    val controller = new Controller(cu, zProject)
    controller.addListener(fireDirtyClosure)
    viewer = new TreeViewer(parent, controller, this)
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
  def setFocus() { viewer.canvas.setFocus }
  def openGUI() {
    if (!shell.isDefined) {
      val newshell = new Shell(getSite.getShell, SWT.MODELESS | SWT.CLOSE | SWT.RESIZE)
      newshell.setLayout(new FillLayout)
      newshell.setText(getTitle + " GUI");
      val guiViewer = new GuiViewer(newshell, controller)
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
    super.dispose()
    controller.removeListener(fireDirtyClosure)
    controller.dispose()
    viewer.dispose()
    shell foreach { s ⇒ if (!s.isDisposed) s.dispose }
  }
  override def gotoMarker(marker: IMarker) {
    val lne = marker.getAttribute(IMarker.LINE_NUMBER).asInstanceOf[Int]
    viewer.gotoMarker(lne)
    this.setFocus
  }
  override def getAdapter(cl: Class[_]) = {
    if (cl == classOf[IGotoMarker]) this
    else if (cl == classOf[ZaluumCompilationUnit]) controller.cu
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