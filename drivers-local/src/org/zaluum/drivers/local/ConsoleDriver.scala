package org.zaluum.drivers.local

import org.zaluum.runtime.{DefaultSink, Sink,Driver,Setup,Util}
import se.scalablesolutions.akka.actor.ActorRef
import scala.collection.mutable.Map
class ConsoleDriver(setup:Setup) extends Driver {
	val map = Map[String, DefaultSink[Int]]()
	setup.registerDriver("console",this)
  def start(realtime:ActorRef)  { }

  def stop {  }
  def getConsoleSink(id : String) : Sink[Int] = Util.cache(id,map) { new DefaultSink[Int]() } 
  
  def commit (){
  	for ((id, sink) <- map) {
  		if (sink.commit) 
  			println ("** " + id + " = " + sink.real)
  	}
  }
  def begin(){}
}