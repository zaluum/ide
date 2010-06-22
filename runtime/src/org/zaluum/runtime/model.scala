package org.zaluum.runtime
import scala.collection.mutable.{Map,MultiMap,HashMap,Set}
import scala.collection.immutable.{Set => ISet}
import java.util.concurrent._
import se.scalablesolutions.akka.actor._
import Debug2Model._
import serial._
import ProtoConversions._
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
  val slot = if (in) box.inPorts.size else box.outPorts.size
	box.add(this)
	
	var connections : Set[Port[A]] = Set()
  def connect(dst : Port[A]):Unit 
	override def toString():String = name + "=" + v
	val fqName = box.fqName + "$" + name
	def in:Boolean
	lazy val proto =  ModelProtos.Port.newBuilder()
        .setName(name)
        .setIn(in)
        .setDirect(false)
        .setLeft(in)
        .setSlot(slot)
        .setType("ttype")
        .setLabel("link")
        .setPosition((0,0))
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
	protected def toProto = {
	  val b = ModelProtos.Box.newBuilder
	  b.setType(ModelProtos.BoxType.SCRIPT)
	  b.setBounds((0,0),(50,50))
	  b.setId(name)
	  val oports = (List() ++ ports.values).sorted(Ordering.by((_:Port[_]).name))
	  oports foreach {p=> b.addPort(p.proto) }
	  b	  
	}
	lazy val proto = toProto.build

}
abstract class ComposedBox(name:String, parent:ComposedBox) extends Box(name,parent){
  val director : Director
	val children : Map[String,Box] = Map()
	def find(names : List[String]):Option[ComposedBox] = names match {
    case Nil => Some(this)
    case head :: tail => children.get(head) match {
      case Some(c : ComposedBox) => c.find(tail)
      case _ => None
    }
  }
	private[runtime] def add(box:Box) = addTemplate(children,box)
	final def act(process:Process):Unit = {director.run(process)} // TODO pattern strategy
  lazy val cproto : ModelProtos.Box = {
	  val box =  ModelProtos.Box.newBuilder(proto)
	  val sorted = (List()++children.values).sorted(Ordering.by((_:Box).name))
	  sorted foreach {b => box.addChild(b.proto)}
	  val connections = {
      var s = ISet[ModelProtos.Line]()
      for (b<-children.values;
        from<- b.ports.values;
        to<- from.connections
        if (to.box.parent ==  ComposedBox.this)) { // TODO internal connections
          val line = ModelProtos.Line.newBuilder()
            /*for (bend <- bendpoints) {
              line.addBendpoint(serial.ModelProtos.Bendpoint.newBuilder.setP1(
                  bend.a).setP2(bend.b).setWeight(1.0f))*/
          def pname(p:Port[_]) =  p.box.name + "#" + p.name
          line.setFrom( pname(from)).setTo(pname(to))
          s+=line.build
      }
      s
    }
    val wsorted = (List()++connections).sorted(Ordering.by((_:ModelProtos.Line).getFrom));
    wsorted foreach {w => box.addWire(w)}
	  box.build
  }
	override lazy val proto : ModelProtos.Box= {
    val box = toProto
    box.setType(ModelProtos.BoxType.COMPOSED);   
    box.build
  }
}
