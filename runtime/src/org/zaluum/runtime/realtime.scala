package org.zaluum.runtime
import java.util.concurrent._
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.config._
import Actor._


sealed abstract class Event 
case class Push(values : List[ValueChange]) extends Event 
case class LoadEvent(model: Model ) extends Event 
case class NewDataEvent(data : Any, dst:Box) extends Event
case class TimeEvent(dst:List[Box]) extends Event
case class DebugModelEvent(fqName:String) extends Event
case class DebugRequest(fqName:Set[String]) extends Event

class WallTime extends Time{
  
  val scheduler = Executors.newSingleThreadScheduledExecutor()
  val startTime = System.nanoTime
  var _currentTime = 0L
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
  def updateNow = _currentTime = System.nanoTime
  def nowNano = _currentTime - startTime
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
    case Push(values) => process.push(values)
    case TimeEvent(boxes) => process.wakeup(boxes)
    case LoadEvent(model) =>  process.load(model); self.reply("ok")
    case DebugModelEvent(str) => process.toDModel(str) foreach { d=> self.reply(d)}
    case DebugRequest(boxes) => self.reply(process.debugData(boxes)) 
  }
}

object RealtimeServer {
  def make  = actorOf[RealTimeActor]
}