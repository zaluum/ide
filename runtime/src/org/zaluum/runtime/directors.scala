package org.zaluum.runtime
import scala.collection.immutable._

abstract class Director(val c:DirectedBox){
  def run(process:Process):Unit
  def activate(box:Box) 
}
class CyclicDirector(c: ComposedBox) extends Director(c){
  var enabled:Boolean = false
  override def run(process:Process)={
    if (enabled){
      for (b <- c.children.values){
        b.act(process)
      }
      for (b<- c.children.values; 
        p<- b.outPorts) {
        p.changed
      }

    }
  }
  def activate(box:Box){}
}
class EventDirector(c:DirectedBox) extends Director(c){
	assert(c!=null)
	assert(c.children!=null)
  private var queued : TreeSet[Box]= TreeSet[Box]()(Ordering.by(_.name)) ++ c.children.values
  override def run(process:Process) {
    while(!queued.isEmpty){
      val b = queued.firstKey
      queued -=b
      b.act(process)
      for (p<- b.outPorts) {
        p.changed
      }
    }
  }
  def activate(box:Box) = queued += box
}