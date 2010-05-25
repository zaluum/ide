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

class MainBox extends ComposedBox(name="main",parent=null) {
  val director = new EventDirector(this)
  override private[runtime] def add(port:Port[_]) = error ("Port cannot be added")
  override def recursiveQueue():Unit = {}
}

trait Process extends Actor {
  val time : ActorRef
  val remote : ActorRef
  var currentTime : Long = System.nanoTime
  var root : MainBox = null
  override def receive = {
    case PushInputEvent(s,v) => s
    case msg : LoadEvent => {
      val a = self.spawn({
          Thread.sleep(4000)
          println(msg)
          self.reply("ok");
          exit
        })
      a forward msg
    }
    case TimeEvent(box) => box
    case NewDataEvent(data,dst) => data
  }
  def reschedule(b:Box, t:Long) = time ! (b,t) 
//  def debugRun() =   while (!eventQueue.isEmpty) process(eventQueue.take())
}
class Time(val p:Process) extends Actor {
  override def receive = { 
    case _ =>
  }
  override def init = {
    println("time")
  }
}
class Remote(val p:Process) extends Actor{
  override def receive = { 
    case _ =>
  }
}
class ProductionServer extends Process{
  self.trapExit = List(classOf[Throwable])
  self.faultHandler = 
    Some(OneForOneStrategy(5, 5000))
  lazy val remote = actorOf(new Remote(this));
  lazy val time = actorOf(new Time(this));
  override def init = {
    self.startLink(remote)
    self.startLink(time)
  }
}
