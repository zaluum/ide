package org.zaluum.runtime
import scala.collection.mutable.Map
import java.util.concurrent._
import scala.concurrent.JavaConversions._
import scala.concurrent.{ops,TaskRunner,SyncVar}
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.config._
import Actor._
trait Model {
  def create(m : MainBox) : Unit
}
sealed abstract class Event 
case class PushInputEvent(s: String, v:Int) extends Event 
case class LoadEvent(model: Model ) extends Event 
case class NewDataEvent(data : Any, dst:Box) extends Event
case class TimeEvent(dst:Box) extends Event
case class DebugModelEvent(fqName:String) extends Event
case class DebugRequest(fqName:String) extends Event

class MainBox extends ComposedBox(name="",parent=null) {
  val director = new EventDirector(this)
  override lazy val fqName:String = ""
  override private[runtime] def add(port:Port[_]) = error ("Port cannot be added")
  override def recursiveQueue():Unit = {}
}
import Debug2Model._
trait Process extends Actor {
  val time : ActorRef
  val remote : ActorRef
  var currentTime : Long = System.nanoTime
  var root : MainBox = null
  override def receive = {
    case PushInputEvent(s,v) => s
    case msg @ LoadEvent(model) => 
      root = new MainBox
      model.create(root)
      self.reply("ok")
    case DebugModelEvent(str) => self.reply(toDModel(str))
    case TimeEvent(box) => box
    case NewDataEvent(data,dst) => data
    case DebugRequest(fqName) => self.reply(debugData(fqName))
  }
  def reschedule(b:Box, t:Long) = time ! (b,t)
  def toDModel(str:String) : Option[serial.ModelProtos.ModelFragment] = {
    for (c <- findComposed(str)) 
      yield {
      val fqName = if (c.parent==null) "" else c.parent.fqName
      serial.ModelProtos.ModelFragment.newBuilder
        .setFragment(c.cproto).setFqName(fqName).build
    }
  }
  def debugData(fqName:String)= {
    for (c<-findComposed(fqName)) yield {
      val d = serial.ModelProtos.DebugValue.newBuilder
      d.setFqName(fqName);
      for (b<- c.children.values; 
        p<- b.ports.values){
          d.addValue(serial.ModelProtos.PortValue.newBuilder
            .setBox(b.name)
            .setPort(p.name)
            .setValue(p.v.toString))
      }
      d.build
    }
  }
  def findComposed(fqName : String) : Option[ComposedBox] = {
    val names  = (List() ++fqName.split("/")) filter {_.size!=0}
    
    if (root!=null){
      val found = root.find(names)
      found
    }
    else None
  }
//  def debugRun() =   while (!eventQueue.isEmpty) process(eventQueue.take())
}
class Time(val p:Process) extends Actor {
  override def receive = { 
    case _ =>
  }
  override def init = {
    println("starting time")
  }
  override def shutdown = {
    println("stopping time")    
  }
}
class Remote(val p:Process) extends Actor{
  override def receive = { 
    case _ =>
  }
  override def init = {
    println("starting remote")
  }
  override def shutdown = {
    println("stopping remote")    
  }
}
object RunServer {
  def make  = actorOf[ProductionServer]
}
class ProductionServer extends Process{
  lazy val remote = actorOf(new Remote(this));
  lazy val time = actorOf(new Time(this));
  override def init = {
    remote.start
    time.start
  }
  override def shutdown = {
    remote.stop
    time.stop
  }
}
