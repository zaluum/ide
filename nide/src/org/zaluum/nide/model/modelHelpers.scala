package org.zaluum.nide.model

import org.zaluum.nide.protobuf.BoxFileProtos
import scala.annotation.tailrec

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
  def toProto  = {
    val p =BoxFileProtos.BoxClassDef.Point.newBuilder
    p.setX(x)
    p.setY(y)
    p.build
  }
}
case class Vector2(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Vector2(x + v.x, y + v.y)
}
object Geometry {
  def maxDim(a: Dimension, b: Dimension) = Dimension(math.max(a.w, b.w), math.max(a.h, b.h))
}
case class Dimension(w: Int, h: Int) {
  def toProto  = {
    val p =BoxFileProtos.BoxClassDef.Point.newBuilder
    p.setX(w)
    p.setY(h)
    p.build
  }
}
trait Positionable {
  def pos: Point
}
trait Resizable extends Positionable {
  def size: Dimension
}
trait Namer {
  def usedNames: Set[String]
  def isNameTaken(str: String): Boolean = usedNames.contains(str)
  def nextName(str: String): String = {
    val digits = str.reverse.takeWhile(_.isDigit).reverse
    val nextValue = if (digits.isEmpty) 1 else digits.toInt + 1
    str.slice(0, str.length - digits.length) + nextValue
  }
  @tailrec
  final def freshName(str: String): String = {
    if (!isNameTaken(str)) str
    else freshName(nextName(str))
  }
}
/*object BoxClassDeclLocator {
  val ConnReg = """\((.*?),(.*?)\)""".r
}
trait BoxClassDeclLocator extends Locatable {
  self: BoxClassDecl ⇒
  def location = Location("%")
  def locate(l: Location): Option[Locatable] = {
    l.str match {
      case s if s.startsWith("(") ⇒ // Connection
        val BoxClassDeclLocator.ConnReg(from, to) = s
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
}*/