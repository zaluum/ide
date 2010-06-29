package org.zaluum.ide.debug
import org.zaluum.ide._
import org.zaluum.runtime.Debug2Model
import org.eclipse.gef.{EditPartFactory,EditPart}
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.gef.ui.actions.GEFActionConstants._
class DebugEditor extends UpEditor{
  val model = new DebugEditParts.ModelUpdater(org.eclipse.swt.widgets.Display.getCurrent)
  def factory = ZaluumDebugFactory
  def getPaletteRoot = Palette()
  override def doSave(p : IProgressMonitor) {}
  override def createActions(){
    super.createActions()
    addAction(new PushAction(this))
  }
  override def fillContextMenu(implicit menu : IMenuManager) {
    super.fillContextMenu(menu)
    addm(GROUP_EDIT,PushAction.ID);
  }
}
import Debug2Model._
object ZaluumDebugFactory extends EditPartFactory {
  import DebugEditParts._
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case model2 : ModelUpdater => new DModelEditPart(model2)
    case cbox : ComposedDBox => new DBoxEditPart(cbox) with ComposedEditPartT 
    case box : DBox => new DBoxEditPart(box)
    case port : DPort => new DPortEditPart(port)
    case wire : DWire => new DWireEditPart(wire)
    case _ => null
  }
}

