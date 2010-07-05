package org.zaluum.ide.debug

import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.jface.util.SafeRunnable;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import org.zaluum.runtime.Debug2Model._
import org.zaluum.runtime._
object PushAction{
  val ID = "PushAction";
  
}
class PushAction(e : DebugEditor)  extends SelectionAction(e) {

  override def init {
    setId(PushAction.ID)
    setText("Push Value")
    setToolTipText("Push value")
    val sharedImages = PlatformUI.getWorkbench.getSharedImages
    setImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_COPY))
    setDisabledImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_COPY_DISABLED))
    setEnabled(false)
  }

  override def calculateEnabled() =  getSelectedObjects().size() == 1 && 
      getSelectedObjects().get(0).isInstanceOf[DebugEditParts.type#DPortEditPart]
  
  override def runWithEvent(event : Event) {
    val shell = getWorkbenchPart().getSite().getShell();
    val selected = getSelectedObjects().get(0).asInstanceOf[DebugEditParts.type#DPortEditPart]
    val pushWireDialog = new ValueSelectDialog(shell,selected.model.ttype);
    if (pushWireDialog.open() == Window.OK
        && pushWireDialog.result != null) {
      doPush(selected.model, pushWireDialog.result);
    }
  }

  def doPush(p:DPort, v:Any) = {}//e.model.push(p ,v)
}
