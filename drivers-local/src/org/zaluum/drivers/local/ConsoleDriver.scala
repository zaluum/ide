package org.zaluum.drivers.local

import org.zaluum.runtime.{Sink,DirtySink,Driver,Setup,Util}
import se.scalablesolutions.akka.actor.ActorRef
import scala.collection.mutable.Map
class ConsoleDriver(setup:Setup) extends Driver {
	val map = Map[String, DirtySink[Int]]()
	setup.registerDriver("console",this)
  def start(realtime:ActorRef)  { }

  def stop {  }
  def getConsoleSink(id : String) : Sink[Int] = Util.cache(id,map) { new DirtySink[Int]() } 
  
  def commit (){
  	for ((id, sink) <- map) {
  		if (sink.dirty) 
  			println ("** " + id + " = " + sink.v)
  	}
  }
}