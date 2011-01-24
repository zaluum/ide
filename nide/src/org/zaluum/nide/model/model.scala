package org.zaluum.nide.model

import org.zaluum.nide.protobuf.BoxFileProtos.Definition.Direction
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.annotation.tailrec

object Model {
  def emptyModel(name: String) = {
    val m = new Model()
    m.className = name
    m
  }
  val ConnReg = """\((.*?),(.*?)\)""".r

}
case class Location(str: String)
trait Locatable {
  def location: Location
}
trait Tuple2 {
  val x: Int
  val y: Int
}
case class Point(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Point(x + v.x, y + v.y)
  def -(o: Point) = Vector2(x - o.x, y - o.y)
  def >>(despl: Int) = Point(x + despl, y)
}
case class Vector2(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Vector2(x + v.x, y + v.y)
}
case class Dimension(w: Int, h: Int)
trait Positionable {
  var pos: Point
}
class Model extends Locatable {
  var className = ""
  var imageName = ""
  var boxes = Set[Box]()
  var portDecls = Set[PortDecl]()
  def boxNamed(str: String) = boxes.exists { _.name == str }
  var connections = Set[Connection]()
  def cleanUp {
    connections = connections.filterNot { c ⇒
      c.from == c.to || // loop
        (c.buf.isEmpty && !(c.from.isDefined && c.from.isDefined))
    } //empty waypoints
  }
  def findBox(str: String) = boxes find { _.name == str }
  def findPortDecl(str: String) = portDecls find { _.name == str }
  def isNameTaken(str: String): Boolean = {
    boxes.exists { _.name == str } || portDecls.exists { _.name == str }
  }
  def nextName(str: String): String = {
    val digits = str.reverse.takeWhile(_.isDigit).reverse
    val nextValue = if (digits.isEmpty) 1 else digits.toInt + 1
    str.slice(0, str.length - digits.length) + nextValue
  }
  @tailrec
  final def nextFreeName(str: String): String = {
    if (!isNameTaken(str)) str
    else nextFreeName(nextName(str))
  }
  def location = Location("%")
  def locate(l: Location): Option[Locatable] = {
    l.str match {
      case s if s.startsWith("(") ⇒ // Connection
        val Model.ConnReg(from, to) = s
        def locateRef(str: String) = locate(Location(str)).asInstanceOf[Option[PortRef]]
        val f = Option(from) flatMap { locateRef(_) }
        val t = Option(to) flatMap { locateRef(_) }
        val res = connections.find { c ⇒ c.from == f && c.to == t }
        res
      case s if s.startsWith("$#") ⇒ // ModelPortRef
        val v = s.substring(2)
        portDecls.find(_.name == v) map { _ ⇒ ModelPortRef(v) }
      case s if s.startsWith("$") ⇒ // BoxPortRef
        val v = s.substring(1).split("#")
        val boxName = v(0)
        val portName = v(1)
        boxes.find(_.name == boxName) map { b ⇒ BoxPortRef(b, portName) }
      case s if s.startsWith("#") ⇒ // PortDecl
        val v = s.substring(1)
        portDecls.find(_.name == v)
      case "%" ⇒ Some(this) // model
      case s ⇒ boxes.find(_.name == s) // box
    }
  }
}

object Box {
  def apply(model: Model, name: String, className: String, pos: Point) = {
    val b = new Box()
    b.className = className
    b.pos = pos
    b.name = name
    model.boxes += b
    b
  }
}

class Box extends Positionable with Locatable {
  var className = ""
  var pos = Point(0, 0)
  var name = ""
  def toProto = {
    val instance = BoxFileProtos.Contents.Instance.newBuilder()
    instance.setName(name)
    instance.setClassName(className)
    instance.setPos(ProtoModel.toPoint(pos))
    instance.build
  }
  def location = Location(name)
  override def toString = name
}
object PortDecl {
  def apply(m: Model, name: String, in: Boolean, pos: Point, posExternal: Point, desc: String): PortDecl = {
    val p = new PortDecl(m, name, in, desc)
    p.pos = pos
    p.posExternal = posExternal
    m.portDecls += p
    p
  }
}
class PortDecl(var m: Model, var name: String, var in: Boolean, var descriptor: String) extends Positionable with Locatable {
  var pos = Point(0, 10)
  var posExternal = Point(0, 0)
  override def toString = "portDecl(" + name + ")"
  def toProto = {
    val port = BoxFileProtos.Definition.Port.newBuilder()
    port.setDirection(if (in) Direction.IN else Direction.OUT)
    port.setPosInternal(ProtoModel.toPoint(pos));
    port.setPosExternal(ProtoModel.toPoint(posExternal))
    port.setName(name)
    port.setType(descriptor)
  }
  def location = Location("#" + name)
}
class BoxClass(val className: String, val scala: Boolean = false, val image: String) {
  var ports = Set[TypedPort]()
  def classNameWithoutPackage = className.split('.').lastOption
  def port(s: String) = ports find { _.name == s }
  override def toString = "boxClass[" + className + "]"
}
case class TypedPort(val descriptor: String, val in: Boolean, val name: String, val pos: Point)

abstract class Command {
  def act() = redo()
  def redo()
  def undo()
  def canExecute: Boolean
}
class ChainCommand(val commands: List[Command]) extends Command {
  override def act() { commands.foreach { _.act() } }
  def redo() { commands.foreach { _.redo() } }
  def undo() { commands.reverse.foreach { _.undo() } }
  def canExecute = commands.forall(_.canExecute)
}
class MoveCommand(positionable: Positionable, pos: Point) extends Command {
  var old = positionable.pos
  def redo() { positionable.pos = pos }
  def undo() { positionable.pos = old }
  def canExecute = true
}
class ConnectCommand(m: Model, c: Connection) extends Command {
  def redo() { m.connections += c }
  def undo() { m.connections -= c }
  def canExecute = {
    c.from.isDefined && c.to.isDefined &&
      // not repeated 
      !m.connections.exists(other ⇒ other.from == c.from && other.to == c.to)
  }
}
/*class ResizeCommand(box: Box, pos:(Int,Int),size: (Int,Int)) extends Command{
  val oldSize = box.size
  val oldPos = box.pos
  def redo() { box.size = size; box.pos = pos }
  def undo() { box.size = oldSize; box.pos = oldPos }
}*/
class CreateCommand(box: Box, model: Model) extends Command {
  def redo { model.boxes += box }
  def undo { model.boxes -= box }
  def canExecute = true
}
class CreatePortDeclCommand(portDecl: PortDecl, model: Model) extends Command {
  def redo { model.portDecls += portDecl }
  def undo { model.portDecls -= portDecl }
  def canExecute = true
}