package org.zaluum.runtime
import java.util.concurrent._
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.config._
import Actor._


sealed abstract class Event 
case class Push(values : List[ValueChange]) extends Event 
case class LoadEvent(modelBuilder: ModelBuilder ) extends Event 
case class ModelReplace(mainBox: MainBox, setup:Setup) extends Event
case class Activate(dst:List[Box]) extends Event
case class DebugModelEvent(fqName:String) extends Event
case class DebugRequest(fqName:Set[String]) extends Event

class WallTime {
  
  val scheduler = Executors.newSingleThreadScheduledExecutor()
  val startTime = System.nanoTime
  var _currentTime = 0L
  var timeQueue = Map[Box,Long]()
  var actor :ActorRef = _
  def commit() {
    for ((box,time) <- timeQueue) {
      scheduler.schedule(new Runnable{
        def run = actor ! Activate(List(box))
      }, time, TimeUnit.MILLISECONDS)
    }
    timeQueue = timeQueue.empty
  }
  def queue(b : Box, t: Long){timeQueue += ((b,t))}
  def updateNow() = _currentTime = System.nanoTime
  def nowNano = _currentTime - startTime
}
trait Driver {
  def start(realtime:ActorRef)
  def stop()
  def commit()
  def begin()
}
class Setup private[runtime]() {
	private[runtime] val drivers = collection.mutable.Map[String,Driver]() 
	def registerDriver(id:String, d:Driver) {
		if (drivers.contains(id) ) throw new Exception("Driver " + id + " already defined")
		drivers(id) = d
	}
}

class RealTimeActor extends Actor{
  val time = new WallTime
  private def begin {
  	time.updateNow()
		if (setup!=null)
			setup.drivers.values foreach (_.begin())

  }
  private def end {  	
		if (setup!=null)
			setup.drivers.values foreach (_.commit())
		time.commit()
  }
  val process = new Process(begin _, end _)
  var setup : Setup = _
  override def init  {
    time.actor = self
    process.run
  }
  override def shutdown  {
  }
  override def receive = {
    case Push(values) => process.push(values)
    case Activate(boxes) => process.wakeup(boxes)
    case ModelReplace(model,setup) =>
    	// process setup
      // TODO hotreplace drivers
    	this.setup = setup
    	setup.drivers.values foreach {_.start(self)};
    	process.load(model)
    case LoadEvent(modelBuilder) => 
    	val me = self
    	Actor.spawn { 
    		val setup = new Setup
    		val mainBox = modelBuilder.create(setup)
    		me ! ModelReplace(mainBox,setup)
    	}
    	self.reply("loading")
    case DebugModelEvent(str) => process.toDModel(str) foreach { d=> self.reply(d)}
    case DebugRequest(boxes) => self.reply(process.debugData(boxes)) 
  }
}

object RealtimeServer {
  def make  = actorOf[RealTimeActor]
}