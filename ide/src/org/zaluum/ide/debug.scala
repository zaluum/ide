package org.zaluum.ide

import org.zaluum.runtime.{Command=>C,Bendpoint=>BP,_}
import org.zaluum.runtime.serial.ModelProtos
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Display
import org.eclipse.draw2d._
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

class ModelUpdater(display:Display) extends VModel {
  println("creating ModelUpdater")
  val process = RunServer.make
  process.start
  def currentBox:Option[ComposedDBox] = synchronized {
    return _currentBox
  }
  private var _currentBox:Option[ComposedDBox] = None 
  val root = new ComposedDBox("main","/",ISet(),null,ISet(),ISet());
  def up() = synchronized {
    for (o <- currentBox)
      moveTo(Option(o.parent))
  }
  def moveTo(o:Option[ComposedDBox]) {}
  
  case object Time
  case class Change(fqName : String)
  val actor = Actor.actorOf(new Actor {
    override def init {
      process !! LoadEvent(new org.zaluum.example.Example())
    }
    def receive = {
      case Time => updatePortValues
      case Change(s:String) => 
        val o : Option[Option[ModelProtos.Box]] = process !! DebugModelEvent(s)
        o match {
          case Some(Some(p)) => setProto(Some(p))
          case None => println("error timeout")
          case _ => println("other")
        }
      case p => error(p.toString)
    }
  })
  var i = 0
  def updatePortValues = synchronized{
    for (c <- _currentBox; b <- c.boxes; p<-b.ports){ 
      
      p.value = ""+i
    }
    i=i+1
    display.asyncExec (notifyPortsLocked _)
  }
  def notifyPortsLocked = synchronized{
    for (c <- _currentBox; b <- c.boxes; p<-b.ports){ 
      p.notifyObservers
      p.vbox .notifyObservers
    }    
  }
  def notifyObserversLocked : Unit = synchronized{
    notifyObservers
  }
  def setProto(oproto : Option[ModelProtos.Box]) = synchronized{
    _currentBox = oproto map {proto => Debug2Model.Deserialize.deserializeComposed(proto)}
    println("set currentBox " + _currentBox)
    display.asyncExec(notifyObserversLocked _)
  }
  actor.start
  actor ! Change("/")
  import java.util.concurrent.TimeUnit
  Scheduler.schedule(actor, Time, 100, 100, TimeUnit.MILLISECONDS)
}
object DebugEditParts extends Parts{
  type B = DBox
  type P = DPort
  type W = DWire
  type C = ComposedDBox
  type M = ModelUpdater
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