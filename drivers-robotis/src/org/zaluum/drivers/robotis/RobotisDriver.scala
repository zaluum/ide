package org.zaluum.drivers.robotis
import scala.collection.mutable.Map
import scala.collection.immutable.{Map => IMap}
import org.zaluum.runtime.{Source,Sink}
import se.scalablesolutions.akka.actor.{Actor,ActorRef}
import org.zaluum.runtime._
import scala.util.control.Exception._


class RobotisDriver(setup : Setup, 
    val refresh : Int = 100, 
    val port : String = "/dev/ttyUSB0") extends Driver{
  
  private case class RobotisPositionSource(id:Int) extends Source[Int]
  private case class RobotisPositionSink(id:Int) extends DirtySink[Int] 
  private case class RobotisTorqueSink(id:Int) extends DirtySink[Int] 
  private case class RobotisFeedbackSource(id:Int) extends Source[RobotisFeedback] 
  private val sourceMap = Map[Int,RobotisPositionSource]()
  private val sinkMap = Map[Int,RobotisPositionSink]()
  private val torqueSinkMap = Map[Int,RobotisTorqueSink]()
  private val feedbackMap = Map[Int,RobotisFeedbackSource]()
  setup.registerDriver("robotis",this)
  
  private var realtime : ActorRef = null
  def positionSourceForId(id : Int) : Source[Int] = Util.cache(id,sourceMap){RobotisPositionSource(id)}
  def feedbackSource(id:Int) : Source[RobotisFeedback] = Util.cache(id,feedbackMap){RobotisFeedbackSource(id:Int)}

  def positionSinkForId(id : Int) : Sink[Int] = Util.cache(id,sinkMap){RobotisPositionSink(id)}
  def torqueSinkForId(id : Int) : Sink[Int] = Util.cache(id,torqueSinkMap){RobotisTorqueSink(id)}
  private case object Work
  private var ax12 : AX12 = null
  private val updater = Actor.actorOf(new Updater)
  private  class Updater extends Actor {
    def checkConnection() {
      if (ax12 == null)
        reconnect()
    }
  	def reconnect() {
  	  try {
  	    try{
  	    if (ax12!=null)
  	      ax12.stop
  	    }
  	    Thread.sleep(100) // wait 100ms to let ax12 restart comms
    	  ax12 = new AX12(port)
    	  ax12.start
    	  //println("connected")
    	 // ax12.enterTossMode()
    	  Thread.sleep(200)
  	  }catch {
  	    case ex : Exception => ex.printStackTrace()
  	  }
  	}
    override def init {
      reconnect()
  	}
  	override def shutdown {
  	  if (ax12 !=null){
  	    ax12.stop
  	    ax12 =null
  	  }
  	}
  	
  	def getValues = { 	
  		var m = IMap[Source[_],Any]()
  	  for (s <- feedbackMap.values)	{
  		  ignoring(classOf[Exception]){
  		   m += s -> ax12.get_servo_feedback(s.id);
  		  }  		
  		}
  		m
  	}
  	
  	def setValues() {
  	  for (vmap <- requestValues(); (sink,value) <- vmap){
        ignoring(classOf[Exception]){
          (sink,value) match {
            case (s:RobotisPositionSink, v:Int) =>
              ax12.set_servo_position(s.id , v)
            case (s:RobotisTorqueSink, v:Int) =>
              ax12.set_torque_enabled(s.id, v!=0)
            case _ =>
          }
        }
  	  }
    }
  	def requestValues() : Option[IMap[Sink[_],Any]] = {
  		realtime !! SinkValuesRequest(Set() ++ sinkMap.values ++ torqueSinkMap.values)
  	}
  	override def receive = {
  		case Work =>
  		  ignoring(classOf[Exception]){
    		  checkConnection()
      		realtime ! SourceValuesEvent(getValues)
      		setValues()
      	}
    		//Thread.sleep(1)
    		//println("work")
    		self ! Work  		
  	}
  }
  def start(realtime : ActorRef) {
  	this.realtime = realtime
    updater.start
    updater ! Work
  }
  def stop() {
   updater.stop 
  }
  def commit(){}
}