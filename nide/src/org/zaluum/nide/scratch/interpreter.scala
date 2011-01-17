
package graystone.zaluum.interpreter

import scala.collection.mutable.MultiMap
import graystone.zaluum.{BoxM,ConnectionBoxM}
import org.zaluum.nide.java.In;
import org.zaluum.nide.java.Out;
import java.lang.reflect.{ Field, Method }
import scala.collection.mutable.{Buffer, HashMap, Set, MultiMap}

abstract class Box(val name:String) {
  val inputs = Buffer[Port]()
  val outputs = Buffer[Port]()
  def act()
  def init(a : Box=>Unit) {a(this)}
  def isReady = inputs forall (_.ready)
  def clean() = inputs foreach (_.ready=false)
  override def toString = name
}

abstract class Port(val name:String)  {
  var box : Box = _
  var ready = false
  def apply() : AnyRef
  def :=(v:AnyRef)
  override def toString = box + "$" + name 
}
trait SimpleFunc {
  def apply()
}

class IInstancePort(name:String, getter:Method, setter:Method, cl : Class[_]) extends Port(name) {
  def f = box.asInstanceOf[IInstanceBox].f
  def apply() = {
    println(getter)
    getter.invoke(f).asInstanceOf[AnyRef]
  }
  def := (v: AnyRef) {
       setter.invoke(f,  v.asInstanceOf[AnyRef])
  }
}
class IInstanceBox(name:String, val f:SimpleFunc) extends Box(name) { 
  { val cl = f.getClass
    val fields = cl.getDeclaredFields
    val inM = cl.getDeclaredFields filter (_.getAnnotation(classOf[In]) != null) toList
    val outM = cl.getDeclaredFields filter (_.getAnnotation(classOf[Out]) != null) toList
    def toProp(l: List[Field]) = {
      for {
        field <- l
        getter <- cl.getMethods find { _.getName == field.getName }
        setter <- cl.getMethods find { _.getName == field.getName + "_$eq" }
      } yield {
        val port = new IInstancePort(field.getName, getter, setter, field.getType)
        port.box =this
        port
      }
    }
    inputs ++= toProp(inM) 
    outputs ++= toProp(outM)
  }
  def act = f()  
}

class SimplePort(name: String,var v:AnyRef) extends Port(name) {
  def :=(v: AnyRef) { this.v = v }
  def apply() = v
}
class IConnectionBox(name:String) extends Box(name) {
  val boxes = Buffer[Box]()
  val connections = new HashMap[Port, Set[Port]]() with MultiMap[Port, Port]
  def act(){
    var ready = Buffer[Box]()
    boxes.foreach { _.clean() }
    def propagate(from:Port) {
      connections(from) foreach { to =>
      to:=from()
      to.ready = true
      if (to.box.isReady) 
        ready += to.box
      }
    }
    inputs.foreach { propagate(_) }
    while(!ready.isEmpty) {
      val b = ready.remove(0)
      println("running "  + b)
      b.act()
      b.outputs foreach { propagate(_) } 
    }
  }
}
