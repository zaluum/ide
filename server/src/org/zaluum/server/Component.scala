package org.zaluum.server
import org.zaluum.runtime._
import se.scalablesolutions.akka.actor.Actor._
import se.scalablesolutions.akka.remote.RemoteServer
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.config._
import se.scalablesolutions.akka.config.ScalaConfig._

class Component {
  
  val server = RealtimeServer.make
  val c =  Supervisor(SupervisorConfig(
       RestartStrategy(OneForOne, 3, 10, List(classOf[Throwable])),
       Supervise(
         server,
         LifeCycle(Permanent)) ::
         Nil))

  val myServer = new RemoteServer
  myServer.start(new java.net.InetSocketAddress("localhost", 9999),classOf[Process].getClassLoader)
  myServer.register("zaluum-service",server)  
	def bindModel(m: Model):Unit = {
    println ("loading model")
    spawn({
      val r = server !! LoadEvent(null)
      println(r)
    });
    println ("model loading in background")
    
	}
	def unbindModel(m : Model):Unit = {
	}
}	
