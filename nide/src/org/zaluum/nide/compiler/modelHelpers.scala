package org.zaluum.nide.compiler

import org.zaluum.nide.protobuf.BoxFileProtos
import scala.annotation.tailrec

trait Tuple2 {
  val x: Int
  val y: Int
}
object Point {
  implicit val ordering = Ordering[(Int,Int)].on[Point](p =>(p.x,p.y))  
}
case class Point(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Point(x + v.x, y + v.y)
  def +(p: Point) = Point(x + p.x,y+ p.y )
  def -(o: Point) = Vector2(x - o.x, y - o.y)
  def >>(despl: Int) = Point(x + despl, y)
  def ^(despl: Int) = Point(x, y+despl)
  def distanceOrto(p:Point) = math.abs(x-p.x) + math.abs(y-p.y) 
  def toProto  = {
    val p =BoxFileProtos.Point.newBuilder
    p.setX(x)
    p.setY(y)
    p.build
  }
  def toVector : Vector2 = Vector2(x,y)
}
case class Vector2(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Vector2(x + v.x, y + v.y)
  def dot(v:Vector2) : Double = x*v.x + y*v.y
  def *(d: Double) = Vector2((x*d).asInstanceOf[Int], (y*d).asInstanceOf[Int])
  def negate = Vector2(-x,-y)
  def toPoint : Point = Point(x,y)
}
object Geometry {
  def maxDim(a: Dimension, b: Dimension) = Dimension(math.max(a.w, b.w), math.max(a.h, b.h))
}
case class Dimension(w: Int, h: Int) {
  def toProto  = {
    val p =BoxFileProtos.Point.newBuilder
    p.setX(w)
    p.setY(h)
    p.build
  }
  def + (v: Vector2) = Dimension(w +v.x,h+v.y)
  import math.max
  def ensureMin(min:Dimension) = Dimension(max(w,min.w) , max(h,min.h))
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