package org.zaluum.nide.eclipse

import org.eclipse.core.resources.{ IResourceDeltaVisitor, IResourceChangeEvent, IResourceDelta, ResourcesPlugin, IResourceChangeListener, IFile }
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.part.{ EditorPart, FileEditorInput }
import org.eclipse.ui.{ IFileEditorInput, IEditorInput }
import org.eclipse.jdt.core.JavaCore
import org.eclipse.ui.IEditorPart

abstract class BaseEditor extends EditorPart {
  abstract class InputChangedTracker extends IResourceChangeListener with IResourceDeltaVisitor {
    def resourceChanged(event: IResourceChangeEvent) {
      if (event.getDelta != null)
        event.getDelta.accept(this);
    }

    def visit(delta: IResourceDelta): Boolean = {
      if (delta != null && delta.getResource == inputFile) {
        inputChanged(delta)
        false
      } else true
    }
    def inputChanged(delta: IResourceDelta)
  }
  protected var ignoreNextChangeDelta = false
  private object resourceTracker extends InputChangedTracker {
    protected def closeEditor(save: Boolean) {
      getSite().getPage().closeEditor(BaseEditor.this, save);
    }
    private def hasMovedToFlag(delta: IResourceDelta) =
      (IResourceDelta.MOVED_TO & delta.getFlags()) != 0

    def inputChanged(delta: IResourceDelta) = {
      delta.getKind match {
        /*case IResourceDelta.CHANGED =>
          if (!ignoreNextChangeDelta) {
            ignoreNextChangeDelta = false
            println("changed")
            EclipseUtils.async(display) { 
              if (!isDirty) closeEditor(false) 
            }
          }*/
        case IResourceDelta.REMOVED ⇒
          //if (hasMovedToFlag(delta)) {
            EclipseUtils.async(display) { 
              closeEditor(false) 
            }
          /*} else {
            val newFile = ResourcesPlugin.getWorkspace()
              .getRoot().getFile(delta.getMovedToPath());
            EclipseUtils.async(display) { 
              setInput(new FileEditorInput(newFile)) 
            }
          }*/
        case _ ⇒
      }

    }
  }
  override protected def setInput(input: IEditorInput) {
    removeListener()
    super.setInput(input)
    getEditorInput match {
      case i: IFileEditorInput ⇒
        i.getFile.getWorkspace.addResourceChangeListener(resourceTracker)
      case _ ⇒
    }
  }
  private def removeListener() {
    getEditorInput match {
      case i: IFileEditorInput ⇒
        i.getFile.getWorkspace.removeResourceChangeListener(resourceTracker)
      case _ ⇒
    }
  }
  override def dispose() {
    super.dispose()
    removeListener()
  }
  def display = getSite.getShell.getDisplay;
  def inputFile = getEditorInput.asInstanceOf[FileEditorInput].getFile
  def isSaveAsAllowed(): Boolean = { false }
  def doSaveAs() {}
  def project = inputFile.getProject
  def jproject = JavaCore.create(project)
  def input = inputFile.getContents(true)
  val fireDirtyClosure: () ⇒ Unit = () ⇒ firePropertyChange(IEditorPart.PROP_DIRTY)
 
}