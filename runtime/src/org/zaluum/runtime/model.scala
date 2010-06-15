package org.zaluum.runtime
import scala.collection.mutable.{Map,MultiMap,HashMap,Set}
import scala.collection.immutable.{Set => ISet}
import java.util.concurrent._
import se.scalablesolutions.akka.actor._
import Debug2Model._
trait Named {
	def name:String
	def fqName : String
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
  def in = true
}
class OutPort[A](name:String, value:A, box:Box) extends Port[A](name,value,box){
  override def connect(dst : Port[A]):Unit = {
    if (!((dst.box.parent == box.parent && dst.isInstanceOf[InPort[_]]) ||
        (dst.box == box.parent && dst.isInstanceOf[OutPort[_]] && dst!=this))) {error("invalid connection")}
    connections += dst
  }
  def in = false
}
abstract class Port[A](val name:String, var v:A, val box:Box) extends Named with Subject{
	box.add(this)
	
	var connections : Set[Port[A]] = Set()
  def connect(dst : Port[A]):Unit 
	override def toString():String = name + "=" + v
	val fqName = box.fqName + "$" + name
	def in:Boolean
	lazy val debug : DPort=  new DPort( name, fqName, Slot(0,false), "ttype", "link", box.debug,in) 
}
abstract class Box(val name:String,val parent:ComposedBox) extends Named with UniqueNamed with Subject{
	val ports:Map[String,Port[_]] = Map()
	val inPorts:Set[Port[_]] = Set()
	val outPorts:Set[Port[_]] = Set()
	lazy val fqName:String = parent.fqName + "/" + name
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
	def act(process:Process):Unit
	def recursiveQueue():Unit = {
	  assert(parent!=null,this)
	  parent.director.queue(this); parent.recursiveQueue()
	}
	lazy val debug = new Debug2Model.DBox( name, fqName, ISet()++ports.values map {_.debug},null) 
}
abstract class ComposedBox(name:String, parent:ComposedBox) extends Box(name,parent){
  val director : Director
	val children : Map[String,Box] = Map()
	private[runtime] def add(box:Box) = addTemplate(children,box)
	final def act(process:Process):Unit = {director.run(process)} // TODO pattern strategy
  lazy val cdebug : ComposedDBox= {
    val boxes = ISet[DBox]() ++ (children.values map {_.debug})
    val dports = ISet[DPort]() ++ (ports.values map {_.debug})
    val connections = {
      var s = ISet[DWire]()
      for {
        b<-children.values
        from<- b.ports.values
        to<- from.connections
      } s+=new Debug2Model.DWire(from.debug ,to.debug, List())
      s
    }
    new Debug2Model.ComposedDBox(name, fqName, dports, null,connections,boxes)
  }
}
