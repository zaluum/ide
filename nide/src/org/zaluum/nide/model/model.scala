package org.zaluum.nide.model

import com.google.common.base.Charsets
import java.io.OutputStreamWriter
import java.io.OutputStream
import java.io.InputStream
import org.zaluum.nide.protobuf.BoxFileProtos
import com.google.protobuf.TextFormat
import scala.collection.JavaConversions._
object Model {
  def toPoint(i:Point) = {
    val p = BoxFileProtos.Contents.Point.newBuilder
    p.setX(i.x)
    p.setY(i.y)
    p.build
  }
  def fromPoint(p:BoxFileProtos.Contents.Point) =  (p.getX,p.getY)
}
object ProtoModel{
  def definitionToProtos(m:Model) = {
    val b =BoxFileProtos.Definition.newBuilder()
    b.setClassName(m.className)
    b.setImageName(m.imageName)
    b.build
  }
  def contentsToProtos(m:Model) = {
    val c = BoxFileProtos.Contents.newBuilder()
    for (b <- m.boxes) { c.addInstance(b.toProto)  }
    for (con <-m.connections ) {  }
    // TODO connections
    c.build
  }
  def writeTo(m:Model,out: OutputStream) {
    definitionToProtos(m).writeDelimitedTo(out)
    contentsToProtos(m).writeDelimitedTo(out)
  }
  def writeTextTo(m:Model,out:OutputStream) {
    val o = new OutputStreamWriter(out,Charsets.UTF_8)
    TextFormat.print(definitionToProtos(m), o);
    TextFormat.print(contentsToProtos(m), o)
    o.flush
  }
  def readDefinition(in:InputStream) = { 
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    new BoxClass(definition.getClassName,false,definition.getImageName)
  }
  def read(in : InputStream) ={
    val model = new Model()
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    model.className = definition.getClassName()
    model.imageName = definition.getImageName()
    val contents = BoxFileProtos.Contents.parseDelimitedFrom(in)
    for (instance <- contents.getInstanceList) {
      val box = new Box
      if (model.boxNamed(instance.getName)) 
        throw new Exception("Box name repeated" + instance.getName)
      box.name = instance.getName
      box.pos = Point(instance.getPos.getX, instance.getPos.getY)
      box.className = instance.getClassName
      model.boxes+=box
      // ports
    }
    var portRefs = Map[Box,Set[PortRef]]() 
    for (connection <- contents.getConnectionList){
      if (connection.hasSource && connection.hasTarget){
        def findBox(str:String) = model.boxes find { _.name == str }
        def findPortRef(box : Box , name : String) = {
          if (!portRefs.contains(box)) portRefs += (box -> Set())
          portRefs(box).find {_.name == name} getOrElse {
            val newRef = PortRef(box,name)
            portRefs += (box -> (portRefs(box) + newRef))
            newRef
          }
        }
        def portOf (proto:BoxFileProtos.Contents.PortRef) = {
          findBox(proto.getBoxName) map { b=> findPortRef(b,proto.getPortName) }
        }
        (portOf(connection.getSource), portOf(connection.getTarget)) match {
          case (Some(from),Some(to)) =>
            model.connections += new Connection(Some(from),Some(to))
          case (f,t) => println("ports not found " + f + " " + t)
        }
      }
    }
    model.cleanUp
    model
  }
}
trait Tuple2 {
  val x : Int
  val y: Int
}
case class Point(x:Int, y:Int) extends Tuple2{
  def + (v: Vector2) = Point(x+v.x,y+v.y)
  def - (o:Point) = Vector2(x-o.x, y-o.y) 
  def >> (despl:Int) = Point(x+despl,y)
}
case class Vector2(x:Int,y:Int) extends Tuple2{
  def + (v:Vector2) = Vector2(x+v.x,y+v.y)
}
case class Dimension(w:Int, h:Int)
trait Positionable {
  var pos :Point
}
class Model {
  var className = ""
  var imageName = ""
  var boxes = Set[Box]()
  var portDecls = Set[PortDecl]() 
  def boxNamed(str:String) = boxes.exists{ _.name == str  }
  var connections =Set[Connection]()
  def cleanUp {
    connections = connections.filterNot {c => c.from == c.to  || // loop
      (c.buf.isEmpty && !(c.from.isDefined && c.from.isDefined))} //empty waypoints
  }
}

object Box {
  def apply(name:String, className:String, pos : Point) ={
    val b = new Box()
    b.className = className
    b.pos = pos
    b.name = name
    b
  }
}

class Box  extends Positionable {
  var className = "img"
  var pos = Point(0, 0)
  var name = ""
 // var ports = Set[Port]()
  def toProto = {
    val instance = BoxFileProtos.Contents.Instance.newBuilder()
    instance.setName(name)
    instance.setClassName(className)
    instance.setPos(Model.toPoint(pos))
    instance.build
  }
  override def toString = name 
}
object PortDecl {
  def apply(m:Model, name:String, pos:(Int,Int) = (0,10)) : PortDecl= {
    val p = new PortDecl(m,name)
    m.portDecls += p
    p.pos = pos
    p
  }
}
class PortDecl(var m:Model,var name:String) {
  var pos = (0,10)
  override def toString = "portDecl(" + name + ")"
}
class BoxClass(val className: String,val scala:Boolean = false, val image:String) {
  var ports = Set[TypedPort]()
  def port(s:String) = ports find {_.name ==s}
  override def toString = "boxClass["+className+"]"
}
case class TypedPort(val descriptor: String, val in: Boolean, val name: String, val pos:(Int,Int))

abstract class Command {
  def act() = redo()
  def redo()
  def undo()
  def canExecute:Boolean
}
class ChainCommand(val commands: List[Command]) extends Command{
  override def act() { commands.foreach { _.act() } }
  def redo() { commands.foreach { _.redo() } }
  def undo() { commands.reverse.foreach { _.undo() } }
  def canExecute = commands.forall( _.canExecute ) 
}
class MoveCommand(box: Box, pos: Point) extends Command{
  var old = box.pos
  def redo() { box.pos = pos }
  def undo() { box.pos = old }
  def canExecute = true
}
class ConnectCommand(m: Model, c:Connection) extends Command {
  def redo() { m.connections += c }
  def undo() { m.connections -= c }
  def canExecute = { 
    c.from.isDefined && c.to.isDefined &&
    // not repeated 
    !m.connections.exists(other => other.from==c.from && other.to==c.to )
  }
}
/*class ResizeCommand(box: Box, pos:(Int,Int),size: (Int,Int)) extends Command{
  val oldSize = box.size
  val oldPos = box.pos
  def redo() { box.size = size; box.pos = pos }
  def undo() { box.size = oldSize; box.pos = oldPos }
}*/
class CreateCommand(box:Box, model:Model) extends Command{
  def redo { model.boxes += box }
  def undo { model.boxes -= box }
  def canExecute = true
}