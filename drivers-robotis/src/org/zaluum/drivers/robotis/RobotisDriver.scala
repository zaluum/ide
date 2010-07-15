package org.zaluum.drivers.robotis
import scala.collection.mutable.{Map,Set}
import scala.collection.immutable.{Map => IMap}
import org.zaluum.runtime.{Source,Sink}
import se.scalablesolutions.akka.actor.{Actor,ActorRef}
import org.zaluum.runtime._
import scala.util.control.Exception._


class RobotisDriver(setup : Setup, 
    val refresh : Int = 100, 
    val port : String = "/dev/ttyUSB0",
    val baud : Int = 57600) extends Driver{
  
  private case class RobotisPositionSource(id:Int) extends DefaultSource[Int]
  private case class RobotisPositionSink(id:Int) extends DefaultSink[Int] 
  private case class RobotisTorqueSink(id:Int) extends DefaultSink[Int] 
  private case class RobotisFeedbackSource(id:Int) extends DefaultSource[RobotisFeedback] 

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
  
  private case object Read
  private case class Write(s:Sink[_], v:Any)

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
  	  	
  	override def receive = {
  		case Read =>
  		  try {
    		  checkConnection()
	        for (s <- feedbackMap.values) {
	          try {
	            s.write(ax12.get_servo_feedback(s.id))
	            realtime ! Activate(List()++s.boxes)
	          } catch {
	            case ex : Exception => 
	              println(ex.toString + "getting servo feedback " + s.id)
	              reconnect()
	          }
	        }
      	}catch{
      	  case ex: Exception => 
      	    println(ex.toString + "data")
            reconnect()
      	}
    		self ! Read  		
  		case Write(sink,value) =>
    		try{
    		  checkConnection()
    		  (sink,value) match {
            case (s:RobotisPositionSink, v:Int) =>
              ax12.set_servo_position(s.id , v)
            case (s:RobotisTorqueSink, v:Int) =>
              ax12.set_torque_enabled(s.id, v!=0)
            case _ =>
          } 
    		}catch{
    		  case ex:Exception => 
    		    println(ex.toString + "writing " + sink)
              reconnect()

    		}
  	}
  }
  def start(realtime : ActorRef) {
  	this.realtime = realtime
    updater.start
    updater ! Read
  }
  def stop() {
   updater.stop 
  }
  def commit(){
    sinkMap.values foreach {s => if (s.commit) updater ! Write(s,s.real)}
  }
  def begin() = {
    sourceMap.values foreach { _.commit } 
    feedbackMap.values foreach { _.commit }
  }
}