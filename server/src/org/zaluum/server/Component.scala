package org.zaluum.server
import org.zaluum.runtime._
class Component {
  val e:EventProcessor = new EventProcessor()
  e.startAndWait()
  
	def bindModel(m: Model):Unit = {
	  e.process.queue(LoadEvent(m))
	}
	def unbindModel(m : Model):Unit = {
	}
}	