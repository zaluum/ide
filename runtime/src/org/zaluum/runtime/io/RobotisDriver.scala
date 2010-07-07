package org.zaluum.runtime.io
import scala.collection.mutable.Map
import org.zaluum.runtime.{Source,Sink}
import se.scalablesolutions.akka.actor.{Actor,ActorRef}
import org.zaluum.runtime._


class RobotisDriver(setup : Setup, 
    val refresh : Int = 100, 
    val port : String = "/dev/ttyUSB1") extends Driver{
  
  case class RobotisPositionSource(id:Int) extends Source[Int]
  case class RobotisPositionSink(id:Int) extends DirtySink[Int] 
  
  val sourceMap = Map[Int,RobotisPositionSource]()
  val sinkMap = Map[Int,RobotisPositionSink]()

  setup.registerDriver("robotis",this)
  var realtime : ActorRef = null
  def positionSourceForId(id : Int) : Source[Int] = Util.cache(id,sourceMap){RobotisPositionSource(id)}
  def positionSinkForId(id : Int) : Sink[Int] = Util.cache(id,sinkMap){RobotisPositionSink(id)}
  case object Work
  val ax12 = new AX12(port)
  val updater = Actor.actorOf(new Updater)
  class Updater extends Actor {
  	override def init {
  		ax12.start
  	}
  	override def shutdown {
  		ax12.stop
  	}
  	
  	def getValues = { 	
  		(for (s <- sourceMap.values) 
  			yield (s,ax12.get_servo_position(s.id))  
  			).toMap[Source[_],Any]
  	}
  	
  	def requestValues() : Option[Map[Sink[_],Any]] = {
  		realtime !! SinkValuesRequest(Set() ++ sinkMap.values)
  	}

  	override def receive = {
  		case Work =>
    		realtime ! SourceValuesEvent(getValues)
    			for (vmap <- requestValues(); (sink,value) <- vmap){ 
    				(sink,value) match {
    					case (s:RobotisPositionSink, v:Int) =>
    						ax12.set_servo_position(s.id , v)
    					case _ =>
    				}
    			}
    		Thread.sleep(100)
    		self ! Work  		
  	}
  }
  def start(realtime : ActorRef) {
  	this.realtime = realtime
    updater.start
  }
  def stop() {
   updater.stop 
  }
  def commit(){}
}