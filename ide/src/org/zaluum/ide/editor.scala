package org.zaluum.ide

import org.eclipse.gef._
import org.eclipse.gef.ui.actions._
import org.eclipse.ui._
import org.zaluum.runtime._
import org.zaluum.example._
import org.eclipse.core.runtime._;

class Editor extends BaseEditor{
  val mainbox = new MainBox()
  new Example().create(mainbox)
  val model : VModel = VModel(mainbox.children.values.head.vbox.asInstanceOf[ComposedVBox])
  override def getPaletteRoot = Palette()
  override def createActions(){
    super.createActions()
    addAction(new UpAction(this))
  }
  def modelEditPart =  getGraphicalViewer.getRootEditPart.getChildren.get(0).asInstanceOf[ModelEditPart]
  def factory = ZaluumFactory  
  override def doSave(p : IProgressMonitor) {}
}
class ZFileEditor extends BaseEditor with FileEditor{
  var model : VModel = _
  def factory = ZaluumFactory
  def getPaletteRoot = Palette()
  def serialize = Serialize.serializeTextStream(model)
  def deserialize (i:java.io.InputStream) {
    model = Deserialize.deserialize(i)
  }
  
  override def setInput(input : IEditorInput){
    super.setInput(input)
    /*   TODO val page = getSite.getWorkbenchWindow.getActivePage
    if(page!=null) {
      val reg = getSite.getWorkbenchWindow.getWorkbench.getPerspectiveRegistry;
      if(PerspectiveUtil.confirmPerspectiveSwitch(getSite().getWorkbenchWindow(), reg.findPerspectiveWithId(BoxPerspective.ID)))
          page.setPerspective(reg.findPerspectiveWithId(BoxPerspective.ID));
    } */
  }

 
}
object UpAction{
  val ID = "org.zaluum.ide.editor.up"
}
class UpAction(e:Editor) extends EditorPartAction(e){
  override protected def  init {
    setId(UpAction.ID);
    setText("Up");
    setToolTipText("Up");
    setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_UP));
    setDisabledImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_UP_DISABLED));
  }
  override protected def calculateEnabled = true
  def editor = getEditorPart.asInstanceOf[Editor]
  override def run {
    editor.modelEditPart.up();
  }

}
