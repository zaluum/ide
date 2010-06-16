package org.zaluum.ide

import org.zaluum.runtime.{Command=>C,Bendpoint=>BP,_}
import org.eclipse.swt.SWT
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
import java.util.ArrayList
import java.util.{List => JList}
import org.eclipse.draw2d.geometry.{Rectangle,Dimension}
import Commands._
import Debug2Model._
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.dispatch._

object LocalDebugModel {
  val process =  RunServer.make
  process.start 
  
  def spawnReturn[T](body: => Option[T]): Future[T] = {
    case object Spawn
    val promise = new DefaultCompletableFuture[T](3000)
    Actor.actorOf(new Actor() {
      def receive = {
        case Spawn =>
        try{
          body match {
            case Some(a) => promise completeWithResult a
            case None => promise completeWithException (None,null)
          }
        } catch {
          case e => promise completeWithException (None, e)
        }
        self.stop
      }
    }).start ! Spawn
    promise
  }
  def awaitActor[T](body : => Option[T]): Option[T]={
    val f = spawnReturn(body)
    f.await
    f.result
  }
}
import scala.concurrent.SyncVar
class ModelUpdater {
  val process = RunServer.make
  process.start
  val model = new VModel {
    val root = null
  }
  
  val currentDBox = new SyncVar[Option[ComposedDBox]]
  case object Time
  case class Change(fqName : String)
  val actor = Actor.init {
    process !! LoadEvent(new org.zaluum.example.Example())
  } receive {
    case Time => /*change currentDBox port values*/
    case Change(s:String) => 
      val o = process !! DebugModelEvent(s)
      o match {
        case v @ Some(p:Option[ComposedDBox]) => currentDBox.set(v)  
        case None => println("error timeout")
      }
  }
  actor.start
  import java.util.concurrent.TimeUnit
  Scheduler.schedule(actor, Time, 100, 100, TimeUnit.MILLISECONDS)
}
class LocalDebugModel(m:Model) extends VModel{
  val root : ComposedDBox=  LocalDebugModel.awaitActor[Option[ComposedDBox]] {
      LocalDebugModel.process !! LoadEvent(m)
      LocalDebugModel.process !! DebugModelEvent("/")
    } match {
      case Some( Some (p : ComposedDBox) ) => p
      case _ => null
    }
}

object DebugEditParts extends Parts{
  type B = DBox
  type P = DPort
  type W = DWire
  type C = ComposedDBox
  type M = LocalDebugModel
  class DModelEditPart(val model:LocalDebugModel) extends ModelEditPart 
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