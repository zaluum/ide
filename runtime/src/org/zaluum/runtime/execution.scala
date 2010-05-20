package org.zaluum.runtime
import scala.collection.mutable.Map
import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.JavaConversions._
import scala.concurrent.{ops,TaskRunner,SyncVar}
import com.google.common.util.concurrent._

trait Model {
  def create(m : MainBox) : Unit
}
sealed abstract class Event {
  def process(ex:Process):Unit
}

case class PushInputEvent[T](p:Port[T], v:T) extends Event {
  override def process(ex:Process) = {
    println("pushing " +v+ " to " + p)
    p.v = v
    p.box.recursiveQueue()
  }
}
case class LoadEvent[T](model: Model ) extends Event {
  override def process(proc: Process) = {
    println("loading")
    val newRoot = new MainBox()
    model.create(newRoot)
    proc.root = newRoot
  }
}
case class NewDataEvent(data : Any, dst:Box) extends Event {
  override def process(ex:Process)  ={}
}

class TimeService(){
  
}
class MainBox extends ComposedBox(name="main",parent=null) {
  val director = new EventDirector(this)
  override private[runtime] def add(port:Port[_]) = error ("Port cannot be added")
  override def recursiveQueue():Unit = {}
}
class Process(){  
  var currentTime : Long = System.nanoTime
  private val eventQueue = new LinkedBlockingQueue[Event]()
  def queue(e : Event) = eventQueue.put(e)
  def takeAndProcess() = process(eventQueue.take)
  var root : MainBox = null
  private def process(e:Event) = {
    currentTime = System.nanoTime
    e.process(this)
    if (root!=null)
      root.director.run()
    //TODO commit execution
  }
  def debugRun() =   while (!eventQueue.isEmpty) process(eventQueue.take())
}
class EventProcessor extends AbstractExecutionThreadService{
  val process:Process = new Process()
  
  override def run() = {
    while(isRunning){
      try { 
        process.takeAndProcess()
      }catch{
        case p => println (p)
      }
    }
  }
}