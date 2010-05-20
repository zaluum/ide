package org.zaluum.runtime
import scala.collection.immutable._
import scala.collection.mutable.{Map,MultiMap,HashMap,Set}

trait Named {
	val name:String
}

trait UniqueNamed {
	protected[runtime] def addTemplate[T <: Named](set: Map[String,T], n : T) {
		if (set.contains(n.name)) 
			error ("duplicated name " + n.name)
		else
			set += n.name -> n		
	}
}

class InPort[A](name:String, value:A, box:Box) extends Port[A](name,value,box){
  override def connect(dst : Port[A]):Unit = {
    if (!(dst.box.parent == box && dst.isInstanceOf[InPort[_]])) { error("invalid connection")}
    // TODO in to out connection
    connections += dst
  }
}
class OutPort[A](name:String, value:A, box:Box) extends Port[A](name,value,box){
  override def connect(dst : Port[A]):Unit = {
    if (!((dst.box.parent == box.parent && dst.isInstanceOf[InPort[_]]) ||
        (dst.box == box.parent && dst.isInstanceOf[OutPort[_]] && dst!=this))) {error("invalid connection")}
    connections += dst
  }
}
abstract class Port[A](val name:String, var v:A, val box:Box) extends Named{
	box.add(this)
	
	var connections : Set[Port[A]] = Set()
  def connect(dst : Port[A]):Unit 
	override def toString():String = name + "=" + v
}

abstract class Box(val name:String,val parent:ComposedBox) extends Named with UniqueNamed{
	val ports:Map[String,Port[_]] = Map()
	val inPorts:Set[Port[_]] = Set()
	val outPorts:Set[Port[_]] = Set()
	
	if (parent!=null) 
		parent.add(this)
	
	def InPort[T](name:String, value:T) = new InPort(name,value,this)
  def OutPort[T](name:String, value:T)  = new OutPort(name,value,this)

	private[runtime] def add(port:Port[_]) = { 
	  addTemplate(ports,port)
	  port match {
	    case port:InPort[_] => inPorts+=port
	    case port:OutPort[_] => outPorts+=port
	  }
	}
	override def toString = name + " (" + (ports.values map { _.toString} mkString(",")) +")"  
	def act():Unit
	def recursiveQueue():Unit = {
	  assert(parent!=null,this)
	  parent.director.queue(this); parent.recursiveQueue()}
}

abstract class ComposedBox(name:String, parent:ComposedBox) extends Box(name,parent){

  val director : Director
	val children : Map[String,Box] = Map()
	private[runtime] def add(box:Box) = addTemplate(children,box)
	final def act():Unit = {director.run()}
}
