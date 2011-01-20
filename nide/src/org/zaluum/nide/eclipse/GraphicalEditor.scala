package org.zaluum.nide.eclipse
import java.io.ByteArrayInputStream
import org.eclipse.core.resources.IFile
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.{ IEditorSite, IEditorInput, IEditorPart }
import org.eclipse.ui.part.{ EditorPart, FileEditorInput }
import org.zaluum.nide.model.ProtoModel
import org.zaluum.nide.zge.{ Viewer, Controller }

class GraphicalEditor extends EditorPart {

  var viewer: Viewer = _
  def controller = viewer.controller
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
    controller.onExecute {
      firePropertyChange(IEditorPart.PROP_DIRTY)
    }
    viewer = new Viewer(parent, controller)
  }

  def setFocus() { viewer.canvas.setFocus }

}