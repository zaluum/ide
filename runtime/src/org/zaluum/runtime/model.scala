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
trait Source[A] {
  val boxes_ = Set[Box]()
  def boxes = synchronized{boxes_}
  def suscribe(box : Box) {synchronized {boxes += box}}
  def v : A
}
class DefaultSource[A] extends Source[A] with CommitValue[A]

trait Sink[A]{
	def write (a:A) : Unit 
}
class DefaultSink[A] extends Sink[A] with CommitValue[A]
trait CommitValue[A] { 
	var v : A = _
	var real : A = _
	def write(a:A) { v = a}
	def commit = { 
		val changed = real != v
		real = v
		changed
	}
}
class InPort[A:Manifest](name:String, box:Box) extends Port[A](name,box){
  override def connect(dst : Port[A]):Unit = {
    if (!(dst.box.parent == box && dst.isInstanceOf[InPort[_]])) { error("invalid connection")}
    // TODO in to out connection
    connections += dst
  }
  def in = true
}
class OutPort[A:Manifest](name:String, box:Box) extends Port[A](name,box){
  override def connect(dst : Port[A]) {
  	
    if (!((dst.box.parent == box.parent && dst.isInstanceOf[InPort[_]]) ||
        (dst.box == box.parent && dst.isInstanceOf[OutPort[_]] && dst!=this))) {error("invalid connection")}
    connections += dst
	}
  def in = false
}
abstract class Port[A : Manifest](val name:String,val box:Box) extends Named with Subject{
	var v : A = _
  val manifest = implicitly[Manifest[A]]
  val slot = if (in) box.inPorts.size else box.outPorts.size
  var forced : Option[A] = None
	box.add(this)
	def effectiveValue = forced.getOrElse(v)
	var connections : Set[Port[A]] = Set()
  def connect(dst : Port[A]):Unit 
	override def toString():String = name + "=" + v
	val fqName = box.fqName + "$" + name
	def changed : Unit = {
    if (in) box.activate
    for ( dst <- connections; if dst.v!=effectiveValue) { 
      dst.v = effectiveValue
      dst.changed 
    }
  }
	def in:Boolean
	lazy val proto = { 
    ModelProtos.Port.newBuilder()
        .setName(name)
        .setIn(in)
        .setDirect(false)
        .setLeft(in)
        .setSlot(slot)
        .setType(manifest.toString)
        .setLabel("")
        .setPosition((0,0))
  }
}
abstract class Box(val name:String,val parent:ComposedBox, var pos : (Int,Int,Int,Int) = (0,0,50,50))extends Named with UniqueNamed with Subject{
	val ports:Map[String,Port[_]] = Map()
	val inPorts:Set[Port[_]] = Set()
	val outPorts:Set[Port[_]] = Set()
	lazy val fqName:String = parent.fqName + "/" + name
	if (parent!=null) 
		parent.add(this)
	def InPort[T](name:String)(implicit m : Manifest[T]) = new InPort[T](name,this)
	def InPort[T](name:String, value:T)(implicit m : Manifest[T]) = {
		val i = new InPort[T](name,this)
		i.v = value
		i
	}
  def OutPort[T](name:String)(implicit m : Manifest[T])  = new OutPort[T](name,this)
  def OutPort[T](name:String, value:T)(implicit m : Manifest[T])  = {
		val o = new OutPort[T](name,this)
		o.v = value
		o
	}
	
	private[runtime] def add(port:Port[_]) = { 
	  addTemplate(ports,port)
	  port match {
	    case port:InPort[_] => inPorts+=port
	    case port:OutPort[_] => outPorts+=port
	  }
	}
	override def toString = name + " (" + (ports.values map { _.toString} mkString(",")) +")"  
	def act(process:Process):Unit = act
	def act {}
	def init(process:Process)  {}
	def activate():Unit = {
	  assert(parent!=null,this)
	  parent.director.activate(this); parent.activate()
	}
	def find(names : List[String]):Option[Box] = names match {
	  case Nil => Some(this)
	  case _ => None
	}
	protected def toProto = {
	  val b = ModelProtos.Box.newBuilder
	  b.setType(ModelProtos.BoxType.SCRIPT)
	  b.setBounds((pos._1,pos._2),(pos._3,pos._4))
	  b.setId(name)
	  val oports = (List() ++ ports.values).sorted(Ordering.by((_:Port[_]).name))
	  oports foreach {p=> b.addPort(p.proto) }
	  b	  
	}
	lazy val proto = toProto.build

}
abstract class ComposedBox(name:String, parent:ComposedBox) extends Box(name,parent){
	val children : Map[String,Box] = Map()
	val director : Director = new EventDirector(this)
	override def find(names : List[String]):Option[Box] = names match {
    case Nil => Some(this)
    case head :: tail => children.get(head) match {
      case Some(b : Box) => b.find(tail)
      case _ => None
    }
  }
	private[runtime] def add(box:Box) = addTemplate(children,box)
	override final def act(process:Process):Unit = {director.run(process)} // TODO pattern strategy
	override final def act {} 
  override lazy val proto : ModelProtos.Box = {
    val box = toProto
    box.setType(ModelProtos.BoxType.COMPOSED);   
	  val sorted = (List()++children.values).sorted(Ordering.by((_:Box).name))
	  sorted foreach {b => box.addChild(b.proto)}
	  val connections = {
      var s = ISet[ModelProtos.Line]()
      for (b<-children.values;
        from<- b.ports.values;
        to<- from.connections) { 
          if (to.box.parent ==  ComposedBox.this) {
            val line = ModelProtos.Line.newBuilder()
              /*for (bend <- bendpoints) {
                line.addBendpoint(serial.ModelProtos.Bendpoint.newBuilder.setP1(
                    bend.a).setP2(bend.b).setWeight(1.0f))*/
            def pname(p:Port[_]) =  p.box.name + "#" + p.name
            line.setFrom( pname(from)).setTo(pname(to))
            s+=line.build
          }else{
            
          }
     }
      s
    }
    val wsorted = (List()++connections).sorted(Ordering.by((_:ModelProtos.Line).getFrom));
    wsorted foreach {w => box.addWire(w)}
	  box.build
  }
}
