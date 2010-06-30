package org.zaluum.ide.debug
import org.zaluum.ide._
import org.zaluum.runtime.Debug2Model
import org.zaluum.runtime.serial.ModelProtos
import org.eclipse.gef.{EditPartFactory,EditPart}
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.action.IMenuManager
import org.eclipse.gef.ui.actions.GEFActionConstants._
import se.scalablesolutions.akka.remote._
import se.scalablesolutions.akka.actor.ActorRef
import org.eclipse.ui.IEditorInput
import org.eclipse.swt.widgets.Display
class DebugEditor extends UpEditor{
  type M = ActorRef
  var model : ActorRef = _
  def factory = ZaluumDebugFactory
  def getPaletteRoot = Palette()
  override def doSave(p : IProgressMonitor) {}
  override def createActions(){
    super.createActions()
    addAction(new PushAction(this))
  }
  override protected def setInput(input : IEditorInput) {
     val cinfo = ConnectionInfo("zaluum-service","localhost",9999)
     model = RemoteClient.actorFor("zaluum-service","localhost",9999,classOf[ModelProtos].getClassLoader)
     model.start
     super.setInput(input);
  }
  override def fillContextMenu(implicit menu : IMenuManager) {
    super.fillContextMenu(menu)
    addm(GROUP_EDIT,PushAction.ID);
  }
}
case class ConnectionInfo(serviceName:String, host:String, port:Int)

import Debug2Model._

object ZaluumDebugFactory extends EditPartFactory {
  import DebugEditParts._
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case process : ActorRef => 
      new DModelEditPart(new DebugEditParts.ModelUpdater(Display.getCurrent,process))
    case cbox : ComposedDBox => new DBoxEditPart(cbox) with ComposedEditPartT 
    case box : DBox => new DBoxEditPart(box)
    case port : DPort => new DPortEditPart(port)
    case wire : DWire => new DWireEditPart(wire)
    case _ => null
  }
}

