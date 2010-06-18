package org.zaluum.ide

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
import Utils._

object DebugEditParts extends Parts{
  type T = Debug2Model.type
  class ModelUpdater(display:Display) extends VModel {
    val process = RunServer.make
    process.start
    def currentView:Viewport = synchronized {_currentView}
    private var _currentView:Viewport = TopView 
    val root = new ComposedDBox("main","",ISet(),null,ISet(),ISet());
    def up() = synchronized { _currentView match {
        case ComposedView(c) => 
          val s = c.fqName.split("/").dropRight(1).mkString("/")
          actor ! Change(s)
        case TopView => 
    }}
    def moveTo(o:Viewport)  = o match {
      case ComposedView(c) => actor ! Change(c.fqName)
      case TopView => actor ! Change("")
    }
    
    case object Time
    case class Change(fqName : String)
    val actor = Actor.actorOf(new Actor {
      override def init {
        process !! LoadEvent(new org.zaluum.example.Example())
      }
      def receive = {
        case Time => updatePortValues
        case Change(s:String) => change(s)
        case p => error(p.toString)
      }
    })
    def change(str : String){
       val o : Option[Option[ModelProtos.ModelFragment]] = process !! DebugModelEvent(str)
       o match {
         case Some(Some(p)) => setProto(Some(p))
         case None => println("error timeout")
         case _ => println("not found")
       }
    }
    var i = 0
    def updatePortValues = synchronized{
      currentView match {
        case ComposedView(c:ComposedDBox) => 
          for (b<- c.boxes; p<- b.ports) p.value ="" +i
        case TopView =>
      }
      i=i+1
      display.asyncExec (notifyPortsLocked _)
    }
    def notifyPortsLocked = synchronized{
      currentView match {
        case ComposedView(c) => 
          for (b<- c.boxes; p<- b.ports) p.notifyObservers
        case TopView =>
      }
    }
    def notifyObserversLocked : Unit = synchronized{
      notifyObservers
    }
    def setProto(oproto : Option[ModelProtos.ModelFragment]) = synchronized{
      oproto foreach {proto => 
        _currentView = ComposedView(Debug2Model.Deserialize.deserialize(proto))
        display.asyncExec(notifyObserversLocked _)
      }
    }
    actor.start
    actor ! Change("/")
    import java.util.concurrent.TimeUnit
    Scheduler.schedule(actor, Time, 100, 100, TimeUnit.MILLISECONDS)
  }

  
  class DModelEditPart(val model:ModelUpdater) extends ModelEditPart 
  class DBoxEditPart(val model:DBox) extends BoxEditPart
  class DPortEditPart(val model:DPort) extends PortEditPart{
    type F = PortDebugFigure
    override def createFigure = new PortDebugFigure
    override def refreshVisuals {
      fig.arrange(model.in,model.slot.left, model.slot.pos, model.name, model.link + " = " + model.value)
    }
  }
  class DWireEditPart(val model:DWire) extends WireEditPart
  class ComposedDBoxEditPart(val model:DBox) extends BoxEditPart with ComposedEditPartT
}
class PortDebugFigure extends PortFigure {
}