package org.zaluum.server
import org.zaluum.runtime._
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.remote.RemoteServer
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.config._
import se.scalablesolutions.akka.config.ScalaConfig._

class Component {
  
  val server = actorOf[ProductionServer]
  val c =  Supervisor(SupervisorConfig(
       RestartStrategy(OneForOne, 3, 10, List(classOf[Exception])),
       Supervise(
         server,
         LifeCycle(Permanent)) ::
         Nil))

  val myServer = new RemoteServer
  myServer.start("localhost", 9999,Some(classOf[Process].getClassLoader))
  myServer.register("zaluum-service",server)  
	def bindModel(m: Model):Unit = {
    println ("loading model")
    spawn({
      val r = server !! LoadEvent(m)
      println(r)
    });
    println ("model loading in background")
    
	}
	def unbindModel(m : Model):Unit = {
	}
}	
