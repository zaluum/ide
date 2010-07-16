package org.zaluum.drivers.robotis
import scala.collection.mutable.{Map,Set,Buffer}
import scala.collection.immutable.{Map => IMap}
import org.zaluum.runtime.{Source,Sink}
import se.scalablesolutions.akka.actor.{Actor,ActorRef}
import org.zaluum.runtime._
import scala.util.control.Exception._


class RobotisDriver(setup : Setup, 
    val refresh : Int = 100, 
    val port : String = "/dev/ttyUSB0",
    val baud : Int = 57600) extends Driver{
  
  private case class RobotisCommandSink(id:Int) extends DefaultSink[RobotisCommand] 
  private case class RobotisFeedbackSource(id:Int) extends DefaultSource[RobotisFeedback] 

  private val sinkMap = Map[Int,RobotisCommandSink]()
  private val sourceMap = Map[Int,RobotisFeedbackSource]()
  
  setup.registerDriver("robotis",this)
  
  private var realtime : ActorRef = null
  def feedbackSource(id:Int) : Source[RobotisFeedback] = Util.cache(id,sourceMap){RobotisFeedbackSource(id:Int)}
  def commandSinkForId(id : Int) : Sink[RobotisCommand] = Util.cache(id,sinkMap){RobotisCommandSink(id)}
   
  private case object Read
  private case class Write(values: List[RobotisCommand])

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
    	  ax12 = new AX12(port,baud)
    	  ax12.start
    	  //println("connected")
    	  ax12.enterTossMode()
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
	        for (s <- sourceMap.values) {
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
  		case w : Write =>
    		try{
    		  checkConnection()
    		  ax12.set_multi_servo_command(w.values);
    		}catch{
    		  case ex:Exception => 
    		    println(ex.toString + "writing " + w)
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
    var b = List[RobotisCommand]()
    sinkMap.values foreach {s => if (s.commit) b = s.real :: b}
    if (!b.isEmpty)
      updater ! Write(b)
  }
  def begin() = {
    sourceMap.values foreach { _.commit } 
    sinkMap.values foreach { _.commit }
  }
}