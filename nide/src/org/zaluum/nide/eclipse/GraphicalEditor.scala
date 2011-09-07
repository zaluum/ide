package org.zaluum.nide.eclipse

import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.commands.ExecutionEvent
import org.eclipse.core.resources.IMarker
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.JavaCore
import org.eclipse.swt.events.DisposeEvent
import org.eclipse.swt.events.DisposeListener
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.SWT
import org.eclipse.ui.contexts.IContextService
import org.eclipse.ui.handlers.HandlerUtil
import org.eclipse.ui.ide.IGotoMarker
import org.eclipse.ui.IEditorInput
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.IEditorSite
import org.eclipse.ui.IWorkbenchPage
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnit
import org.zaluum.nide.zge.Controller
import org.zaluum.nide.zge.GuiViewer
import org.zaluum.nide.zge.PaletteView
import org.zaluum.nide.zge.TreeViewer
import org.zaluum.nide.zge.Item
import org.zaluum.nide.zge.ValDefItem
import org.eclipse.jface.viewers.StructuredSelection

class GraphicalEditor extends BaseEditor with IGotoMarker {

  var viewer: TreeViewer = _
  var shell: Option[Shell] = None
  def controller = viewer.controller
  private var selectionListeners = List[() ⇒ Unit]()
  def addSelectionListener(l: () ⇒ Unit) {
    if (!selectionListeners.contains(l)) selectionListeners ::= l
  }
  def removeSelectionListener(l: () ⇒ Unit) {
    selectionListeners = selectionListeners filterNot (_ == l)
  }
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

    getEditorSite().setSelectionProvider(selectionProvider);
  }

  def isDirty(): Boolean = { controller.isDirty }
  def zproject = controller.zproject
  def createPartControl(parent: Composite) {
    val cu = JavaCore.createCompilationUnitFrom(inputFile)
    val zProject = ZaluumProjectManager.getZaluumProject(jproject)
    val controller = new Controller(cu, zProject, parent.getDisplay)
    controller.addListener(fireDirtyClosure)
    viewer = new TreeViewer(parent, controller, this)
    controller.registerViewer(viewer)
    // the first viewer.refresh comes from onResize
    // TODO reopen
  }
  var selected: Option[Item] = None
  def selection = selected map { i ⇒ new StructuredSelection(i) } getOrElse { StructuredSelection.EMPTY }
  def setSelection(i: Option[Item]) {
    if (selected != i) {
      selected = i
      selectionListeners foreach { _() }
      val sel = i match {
        case Some(i) ⇒ SelectionProvider.adaptItem(i, controller)
        case None    ⇒ StructuredSelection.EMPTY
      }
      selectionProvider.setSelection(sel)
    }
  }
  private lazy val selectionProvider = new SelectionProvider()
  def setFocus() {
    showPalette()
    viewer.canvas.setFocus
  }
  def showPalette() {
    try {
      getSite().getPage().showView(PaletteView.ID, null, IWorkbenchPage.VIEW_VISIBLE);
    } catch { case e ⇒ } // throws an exception if invoked while the workbench is loaded
  }
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
      case _                        ⇒
    }
    null
  }
}