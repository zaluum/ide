package org.zaluum.ide

import org.eclipse.gef._
import org.eclipse.gef.ui.actions._
import org.eclipse.ui._
import org.zaluum.runtime._
import org.zaluum.example._
import org.eclipse.core.runtime._;

trait UpEditor extends BaseEditor{
 override def createActions(){
    super.createActions()
    addAction(new UpAction(this))
  }
  def modelEditPart =  super.getGraphicalViewer.getRootEditPart.getChildren.get(0).asInstanceOf[ModelEditPart] 
}
class Editor extends UpEditor{
  val mainbox = new MainBox()
  new Example().create(mainbox)
  val model : VModel = VModel(mainbox.children.values.head.vbox.asInstanceOf[ComposedVBox])
  override def getPaletteRoot = Palette()
  def factory = ZaluumFactory  
  override def doSave(p : IProgressMonitor) {}
}
class ZFileEditor extends UpEditor with FileEditor{
  var model : VModel = _
  def factory = ZaluumWriteFactory
  def getPaletteRoot = Palette()
  import com.google.common.base.Charsets
  def serialize = new java.io.ByteArrayInputStream(
      model.root.asInstanceOf[ComposedPBox].toProto
        .build.toString.getBytes(Charsets.UTF_8)
        )
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
class UpAction(e:UpEditor) extends EditorPartAction(e){
  override protected def  init {
    setId(UpAction.ID);
    setText("Up");
    setToolTipText("Up");
    setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_UP));
    setDisabledImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(ISharedImages.IMG_TOOL_UP_DISABLED));
  }
  override protected def calculateEnabled = true
  def editor = getEditorPart.asInstanceOf[UpEditor]
  override def run {
    editor.modelEditPart.up();
  }

}
