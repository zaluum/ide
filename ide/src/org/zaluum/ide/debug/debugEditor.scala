package org.zaluum.ide.debug
import org.zaluum.ide._
import org.zaluum.runtime.Debug2Model
import org.zaluum.runtime.serial.ModelProtos
import org.eclipse.gef.{EditPartFactory,EditPart}
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.action.IMenuManager
import org.eclipse.gef.ui.actions.GEFActionConstants._
/*import se.scalablesolutions.akka.remote._
import se.scalablesolutions.akka.actor.{ActorRef,Actor}*/
import org.eclipse.ui.IEditorInput
import org.eclipse.swt.widgets.Display
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jface.dialogs.ProgressMonitorDialog
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.ui.PlatformUI

class DebugEditor extends UpEditor{
  type M = (Debug2Model.ComposedDBox, DebugConnection)
  var model : (Debug2Model.ComposedDBox, DebugConnection) = _
  def factory = ZaluumDebugFactory
  def getPaletteRoot = Palette()
  override def doSave(p : IProgressMonitor) {}
  override def createActions(){
    super.createActions()
    addAction(new PushAction(this))
  }
  override protected def setInput(input : IEditorInput) {
     val cinfo = ConnectionInfo("zaluum-service","localhost",9999)
     val irp = new IRunnableWithProgress() {
        override def run(monitor : IProgressMonitor){
          monitor.beginTask("Connecting", 2);
      /*    val conn = new DebugConnection
          monitor.worked(1)
          model = (conn.contents.getOrElse{
            throw new Exception("Cannot connect server")
          }, conn)*/
        }
     }
     try{
       new ProgressMonitorDialog(PlatformUI.getWorkbench.getActiveWorkbenchWindow.getShell)
         .run(false, false, irp);
     }catch{
       case ex => throw new Exception("Connection refused. Close and open to retry",ex)
     }
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
    case (box:ComposedDBox,conn:DebugConnection) => 
      new DModelEditPart(new DebugEditParts.DebugModel(box, conn))
    case cbox : ComposedDBox => new DBoxEditPart(cbox) with ComposedEditPartT 
    case box : DBox => new DBoxEditPart(box)
    case port : DPort => new DPortEditPart(port)
    case wire : DWire => new DWireEditPart(wire)
    case _ => null
  }
}

