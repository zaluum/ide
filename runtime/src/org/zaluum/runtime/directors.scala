package org.zaluum.runtime
import scala.collection.immutable._

abstract class Director(val c:ComposedBox){
  def run():Unit
  def queue(b:Box):Unit
  protected def propagate(src : Port[_]) = {
    for (p <- src.connections) {
      val dst = p.asInstanceOf[Port[Any]]
      if (dst.v!=src.v ) {
        dst.v = src.v
        if (p.box.parent ==c) // only queue my boxes
          queue(p.box)
      }
    }
  } 

}
class CyclicDirector(c: ComposedBox) extends Director(c){
  var enabled:Boolean = false
  override def queue(b : Box) = {}
  override def run()={
    if (enabled){
      for (p <- c.inPorts) propagate(p)
      for (b <- c.children.values){
        b.act()
        for (p <- b.outPorts) propagate(p) 
      }
    }
  }
}
class EventDirector(c:ComposedBox) extends Director(c){
  private var queued = TreeSet[Box]()(Ordering.by(_.name)) //++ c.children.values
  // PRE port values are in equillibrium
  override def run() {
    println("+" + c + " running");
    for (p <- c.inPorts) {propagate(p)}
    while(!queued.isEmpty){
      val b = queued.firstKey
      queued -=b
      b.act()
      for (p <- b.outPorts) {propagate(p)} 
    }
    println("+" + c + " ran");

  }
  
  def queue(b:Box){
    assert (b.parent==c)       
    queued += b
  }
}