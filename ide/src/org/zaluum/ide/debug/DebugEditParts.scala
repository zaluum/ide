package org.zaluum.ide.debug

import org.zaluum.ide._
import org.zaluum.runtime.{Command=>C,Bendpoint=>BP,_}
import org.zaluum.runtime.serial.ModelProtos
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Display
import org.eclipse.core.runtime._
import org.eclipse.gef._
import commands._
import editpolicies._
import rulers._
import requests._
import ui.parts._
import ui.actions._
import palette._
import editparts._
import org.eclipse.jface.resource._
import org.eclipse.ui._
import views.properties._
import org.eclipse.help.IContextProvider
import org.eclipse.swt.graphics._
import scala.collection.JavaConversions._
import scala.collection.mutable._
import scala.collection.immutable.{Set=>ISet}
import java.util.ArrayList
import java.util.{List => JList}
import org.eclipse.draw2d.geometry.{Rectangle,Dimension}
import Commands._
import Debug2Model._
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.dispatch._
import org.zaluum.ide.Utils._
import se.scalablesolutions.akka.remote._
import org.zaluum.runtime._
import java.util.concurrent.TimeUnit


object DebugEditParts extends Parts{
  type T = Debug2Model.type
  
  class ModelUpdater(display:Display) extends VModel {
    case class Time(i:Int)
    case class Change(fqName : String)
    private val process = RemoteClient.actorFor("zaluum-service","localhost",9999,classOf[ModelProtos].getClassLoader)
    
    private val actor = Actor.actorOf(new Actor {
      override def init {
        process.start 
      }
      override def shutdown{
        process.stop
      }
      def receive = {
        case Time(i) => synchronized {currentView foreach {c=>requestData(c.fqName)}}
        case d:ModelProtos.DebugValue => synchronized {updatePortValues(d)}
        case Change(s:String) => synchronized {change(s)}
        
        case p => println("other" + p)
        
      }
      private def requestData(fqName : String){
        val value: Option[ModelProtos.DebugValue] = process !! DebugRequest(fqName)
        for (v <- value)
          updatePortValues(v)
      }
      private def change(str : String) {
        val o : Option[ModelProtos.ModelFragment] = process !! DebugModelEvent(str)
        o match {
          case Some(p) => setProto(Some(p))
          case None => println("error timeout")
          case _ => println("not found")
      }
      }
    })
    @volatile var currentView:Viewport = TopView 
    val root = new ComposedDBox("main","",ISet(),null,ISet(),ISet());
    
    private def swtRun (f : => Unit) {
      display.asyncExec(new Runnable{ 
        def run = if (!display.isDisposed) synchronized(f)
        })
    }
    def up() = currentView match {
      case ComposedView(c) => 
        val s = c.fqName.split("/").dropRight(1).mkString("/")
        actor ! Change(s)
      case TopView => 
    }
    def moveTo(o:Viewport)  = o match {
      case ComposedView(c) => actor ! Change(c.fqName)
      case TopView => actor ! Change("")
    }   
    def push(p:DPort, v:Any) {
      process ! Push(List(PushValue(p.fqName , v)))
    }
    private def updatePortValues(data : ModelProtos.DebugValue) = currentView match {
        case ComposedView(c:ComposedDBox) =>
          if (c.fqName==data.getFqName){
            val updatedPorts = Buffer[DPort]()
            for (d <- data.getValueList; 
              b <- c.boxes.find(_.name == d.getBox);
              p <- b.ports.find(_.name == d.getPort)) {
                if (p.value != d.getValue) {
                    p.value =d.getValue
                    updatedPorts += p
                }
            }
            swtRun { 
              for (p<-updatedPorts) {
                p.notifyObservers
              }
            }
          }else println("old data" + data)
        case TopView =>
    }
    private def setProto(oproto : Option[ModelProtos.ModelFragment]) = synchronized{
      oproto foreach {proto => 
        currentView = ComposedView(Debug2Model.Deserialize.deserialize(proto))
        swtRun {notifyObservers}
      }
    }
    def stop {
      Scheduler.unschedule(actor)
      actor.stop
    }
    def start {
      actor.start
      actor ! Change("/")
      Scheduler.schedule(actor, Time(1), 100, 100, TimeUnit.MILLISECONDS)
    }
  }

  
  class DModelEditPart(val model:ModelUpdater) extends ModelEditPart {
    override def activate {
      super.activate
      model.start
    }
    override def deactivate {
      model.stop
      super.deactivate
    }
  }
  class DBoxEditPart(val model:DBox) extends BoxEditPart
  class DPortEditPart(val model:DPort) extends PortEditPart with OpenPart{
    type F = PortDebugFigure
    override def createFigure = new PortDebugFigure
    override def refreshVisuals {
      fig.arrange(model.in,model.slot.left, model.slot.pos, model.name, model.link + " = " + model.value)
    }
    override def doOpen{
      
    }
  }
  class DWireEditPart(val model:DWire) extends WireEditPart
  class ComposedDBoxEditPart(val model:DBox) extends BoxEditPart with ComposedEditPartT
}
class PortDebugFigure extends PortFigure {
}


