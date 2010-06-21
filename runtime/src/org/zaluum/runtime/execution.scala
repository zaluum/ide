package org.zaluum.runtime
import scala.collection.mutable.{Map,Set}
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
case class TimeEvent(dst:List[Box]) extends Event
case class DebugModelEvent(fqName:String) extends Event
case class DebugRequest(fqName:String) extends Event

class MainBox extends ComposedBox(name="",parent=null) {
  val director = new EventDirector(this)
  override lazy val fqName:String = ""
  override private[runtime] def add(port:Port[_]) = error ("Port cannot be added")
  override def recursiveQueue():Unit = {}
}
import Debug2Model._

class Process (val time:Time) {
  var root : Option[MainBox] = None
  
  def run {
    for (r<-root) {
      r.director.run(this)
    }
    time.commit
  }
  def load (model:Model){
    root = Some(new MainBox)
    model.create(root.get)
    def visit(b:Box) : Unit = b match{
      case c:ComposedBox =>
        c.children.values foreach {child =>visit(child)}
        c.recursiveQueue
      case _ => b.recursiveQueue
    }
    visit(root.get)
    run
  }
  def wakeup(boxes:List[Box]){
    boxes.foreach { b=> b.recursiveQueue}
    run
  }
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
    root match {
      case Some(r) =>  r.find(names)
      case None => None
    }
  }
//  def debugRun() =   while (!eventQueue.isEmpty) process(eventQueue.take())
}
trait Time {
  def commit 
  def queue(b : Box, t: Long)
  def now : Long
}
class WallTime extends Time{
  
  val scheduler = Executors.newSingleThreadScheduledExecutor()
  val startTime = System.nanoTime
  var timeQueue = Map[Box,Long]()
  var actor :ActorRef = _
  def commit {
    for ((box,time) <- timeQueue) {
      scheduler.schedule(new Runnable{
        def run = actor ! TimeEvent(List(box))
      }, time, TimeUnit.MILLISECONDS)
    }
    timeQueue = timeQueue.empty
  }
  def queue(b : Box, t: Long){timeQueue += ((b,t))}
  def now : Long = 0

}

class RealTimeActor extends Actor{
  val time = new WallTime
  val process = new Process(time)
  override def init  {
    time.actor = self
    process.run
  }
  override def shutdown  {
  }
  override def receive = {
    case NewDataEvent(data,dst) => error("not implemented")
    case PushInputEvent(s,v) => error("not implemented")
    case TimeEvent(boxes) => process.wakeup(boxes)
    case LoadEvent(model) =>  process.load(model); self.reply("ok")
    case DebugModelEvent(str) => self.reply(process.toDModel(str))
    case DebugRequest(fqName) => self.reply(process.debugData(fqName))
  }
}

object RunServer {
  def make  = actorOf[RealTimeActor]
}