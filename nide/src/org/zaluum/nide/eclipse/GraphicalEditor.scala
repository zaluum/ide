package org.zaluum.nide.eclipse

import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Shell
import org.eclipse.jface.dialogs.Dialog
import org.zaluum.nide.zge.GUIViewer
import org.zaluum.nide.zge.GUIModel
import org.zaluum.nide.zge.GUIController
import org.zaluum.nide.model.Location
import org.eclipse.ui.ide.IGotoMarker
import org.eclipse.core.resources.IMarker
import java.io.ByteArrayInputStream
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.{ IEditorSite, IEditorInput, IEditorPart }
import org.eclipse.ui.part.{ EditorPart, FileEditorInput }
import org.zaluum.nide.model.ProtoModel
import org.zaluum.nide.zge.{ Viewer, Controller }

class GraphicalEditor extends EditorPart with IGotoMarker {

  var viewer: Viewer = _
  var guiViewer : GUIViewer = _
  var dialog : Dialog = _ 
  def controller = viewer.controller
  def modelView = viewer.modelView
  def doSave(monitor: IProgressMonitor) {
    val in = new ByteArrayInputStream(ProtoModel.toByteArray(viewer.model))
    inputFile.setContents(in, true, true, monitor);
    controller.markSaved()
    firePropertyChange(IEditorPart.PROP_DIRTY)
  }

  def doSaveAs() {}

  def init(site: IEditorSite, input: IEditorInput) {
    setSite(site)
    setInput(input)
    setPartName(inputFile.getName)
  }

  def isDirty(): Boolean = { controller.isDirty }

  def isSaveAsAllowed(): Boolean = { false }

  def inputFile = getEditorInput.asInstanceOf[FileEditorInput].getFile
  def input = inputFile.getContents(true)
  def createPartControl(parent: Composite) {
    val bcp = new EclipseBoxClasspath(inputFile.getProject)
    bcp.update()
    val className = bcp.toClassName(inputFile).getOrElse { "NotFound" }
    val model = ProtoModel.read(input, className)
    input.close()
    val controller = new Controller(model, bcp)
    controller.addListener(fireDirty)
    viewer = new Viewer(parent, controller)
    var shell = new Shell(parent.getShell, SWT.MODELESS | SWT.SHELL_TRIM)
    shell.setLayout(new FillLayout)
    val guiController = new GUIController(new GUIModel,model,bcp)
    guiViewer= new GUIViewer(shell, guiController)
    shell.setSize(400, 300)
    shell.layout()
    shell.open()
//    dialog.setBlockOnOpen(false)
  //  dialog.open
  }
  val fireDirty: () ⇒ Unit = () ⇒ firePropertyChange(IEditorPart.PROP_DIRTY)
  def setFocus() { viewer.canvas.setFocus }
  override def dispose() {
    controller.removeListener(fireDirty)
    viewer.dispose()
  }
  override def gotoMarker(marker: IMarker) {
    val str = marker.getAttribute("BLAME").asInstanceOf[String]
    modelView.gotoMarker(Location(str))
    setFocus
  }
  override def getAdapter(cl: Class[_]) = {
    if (cl == classOf[IGotoMarker]) this
    else super.getAdapter(cl)
  }
}