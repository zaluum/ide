package org.zaluum.ide.debug

import org.zaluum.runtime.serial.ModelProtos
import org.zaluum.runtime.Debug2Model._
import org.zaluum.runtime.{DebugModelEvent,DebugRequest,Push,Debug2Model,PushValue}
import se.scalablesolutions.akka.remote.RemoteClient
import se.scalablesolutions.akka.actor.Actor
import se.scalablesolutions.akka.actor.ActorRef
import se.scalablesolutions.akka.actor.Scheduler
import se.scalablesolutions.akka.remote.MessageSerializer
import java.util.concurrent.TimeUnit
import java.util.concurrent.ConcurrentSkipListSet
import java.util.concurrent.ConcurrentMap
import org.zaluum.ide.Utils._
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map,Buffer}
import com.google.common.collect.MapMaker

class DebugConnection {
  case object Refresh
  case object Wait
  private val process = RemoteClient.actorFor("zaluum-service","localhost",9999,classOf[ModelProtos.DebugResponse].getClassLoader)
  MessageSerializer.setClassLoader(classOf[ModelProtos].getClassLoader)
  private val boxesToUpdate = Map[String,(DBox,Int)]()

  val waitActor = Actor.actor {
    case Wait => Thread.sleep(100); refresh ! Refresh 
  }
  
  val refresh : ActorRef = Actor.actor{
    case Refresh => 
      val s = DebugConnection.this.synchronized{Set() ++ boxesToUpdate.keys}
      val odr = (process !! DebugRequest(s)).asInstanceOf[Option[ModelProtos.DebugResponse]]
      val ports= Buffer[DPort]()
      for (dr <- odr; v <- dr.getValueList){
        DebugConnection.this.synchronized{boxesToUpdate.get(v.getBoxFQ)} match {
          case Some((box,portValue)) if (box != null) =>
            for (portValue <- v.getPortValueList;
            port <- box.ports.find{_.name==portValue.getPortName}){
              port.value = portValue.getValue
              ports += port
            }
          case _ => println(v.getBoxFQ  + " not found")
        }        
        inSWT{ ports foreach { _.notifyObservers} }(null)
      }
      
      waitActor ! Wait
  }
  
  def suscribe(b : DBox) = synchronized{
    boxesToUpdate.get(b.fqName) match {
      case Some((box,i)) => boxesToUpdate.put(b.fqName,(box,i+1))
      case None => boxesToUpdate.put(b.fqName,(b,1))
    }
  }
  def unsuscribe(b : DBox) = synchronized{
    boxesToUpdate.get(b.fqName) match {
      case Some((box,1)) => boxesToUpdate.remove(b.fqName)
      case Some((box,i)) => boxesToUpdate.put(b.fqName, (box,i-1))
      case None => 
    }
  }
  process.start
  waitActor.start
  refresh.start
  refresh ! Refresh
  
  def contents = {
      val frag = awaitActor{(process !! DebugModelEvent("/")).asInstanceOf[Option[ModelProtos.ModelFragment]]}
      frag map {p => Debug2Model.Deserialize.deserialize(p)}
  }
  
  def push(port:DPort, v:Any) {
    process ! Push(List(PushValue(port.fqName,v)))
  }
  def stop {
   process.stop
   waitActor.stop
   refresh.stop
  }
}