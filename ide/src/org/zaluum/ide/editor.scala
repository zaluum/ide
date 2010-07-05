package org.zaluum.ide

import org.eclipse.gef._
import org.eclipse.gef.ui.actions._
import org.eclipse.ui._
import org.zaluum.runtime._
//import org.eclipse.core.runtime.;
import PersistentModel._
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

trait UpEditor extends BaseEditor{
 override def createActions(){
    super.createActions()
    addAction(new UpAction(this))
  }
  def modelEditPart =  super.getGraphicalViewer.getRootEditPart.getChildren.get(0).asInstanceOf[Parts#ModelEditPart] 
}

class ZFileEditor extends UpEditor with FileEditor with StackChangeDirtyFile{
  type M = PersistentEditParts.PModel
  var model : PersistentEditParts.PModel = _
  lazy val outlinePage = new ZaluumOutlinePage(this)
  def factory = ZaluumWriteFactory
  def getPaletteRoot = Palette()
  import com.google.common.base.Charsets
  def serialize = new java.io.ByteArrayInputStream({
      val s = model.asInstanceOf[PersistentEditParts.PModel].root.asInstanceOf[ComposedPBox].toProto
        .build.toString
      println(s)
      s.getBytes(Charsets.UTF_8)})
  def deserialize (i:java.io.InputStream) {
    model = new PersistentEditParts.PModel(Deserialize.deserialize(i))
  }
  override def getAdapter(c : Class[_]) : Object = {
    if(c == classOf[IContentOutlinePage]) outlinePage 
    else super.getAdapter(c)
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
    editor.modelEditPart.model.up();
  }

}
