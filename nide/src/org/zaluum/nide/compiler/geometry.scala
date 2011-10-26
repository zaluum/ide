package org.zaluum.nide.compiler

import org.zaluum.nide.protobuf.ZaluumProtobuf

trait Tuple2 {
  val x: Int
  val y: Int
}
object Point {
  implicit val ordering = Ordering[(Int, Int)].on[Point](p ⇒ (p.x, p.y))
}
case class Point(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Point(x + v.x, y + v.y)
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(o: Point) = Vector2(x - o.x, y - o.y)
  def >>(despl: Int) = Point(x + despl, y)
  def ^(despl: Int) = Point(x, y + despl)
  def distanceOrto(p: Point) = math.abs(x - p.x) + math.abs(y - p.y)
  def toProto = {
    val p = ZaluumProtobuf.Point.newBuilder
    p.setX(x)
    p.setY(y)
    p.build
  }
  def toVector: Vector2 = Vector2(x, y)
  def toDimension = Dimension(x, y)
}
object Vector2 {
  val empty = Vector2(0, 0)
}
case class Vector2(x: Int, y: Int) extends Tuple2 {
  def +(v: Vector2) = Vector2(x + v.x, y + v.y)
  def dot(v: Vector2): Double = x * v.x + y * v.y
  def *(d: Double) = Vector2((x * d).asInstanceOf[Int], (y * d).asInstanceOf[Int])
  def negate = Vector2(-x, -y)
  def toPoint: Point = Point(x, y)
}
object Geometry {
  def maxDim(a: Dimension, b: Dimension) = Dimension(math.max(a.w, b.w), math.max(a.h, b.h))
}
case class Dimension(w: Int, h: Int) {
  def toProto = {
    val p = ZaluumProtobuf.Point.newBuilder
    p.setX(w)
    p.setY(h)
    p.build
  }
  def +(v: Vector2) = Dimension(w + v.x, h + v.y)
  def toVector = Vector2(w, h)
  import math.max
  def ensureMin(min: Dimension) = Dimension(max(w, min.w), max(h, min.h))
}
trait Positionable {
  def pos: Point
}
trait Resizable extends Positionable {
  def size: Dimension
}
object Rect {
  def apply(p: Point, d: Dimension): Rect =
    Rect(p.x, p.y, d.w, d.h)
  val empty = Rect(0, 0, 0, 0)
}
case class Rect(x: Int, y: Int, w: Int, h: Int) {
  def left = x
  def top = y
  def right = x + w
  def bottom = y + h
  def dim = Dimension(w, h)
  def pos = Point(x, y)
  def leftOf(b: Rect) = right <= b.left
  def rightOf(b: Rect) = left >= b.right
  def aboveOf(b: Rect) = bottom <= b.top
  def belowOf(b: Rect) = top >= b.bottom
  def intersectsX(b: Rect) = !leftOf(b) && !rightOf(b)
  def intersectsY(b: Rect) = !aboveOf(b) && !belowOf(b)
  def intersects(b: Rect) = intersectsX(b) && intersectsY(b)
  def isEmpty = w == 0 && h == 0
  def isEmptyW = w == 0
  def isEmptyH = h == 0
  def union(b: Rect): Rect = {
    val (nleft, nw) = if (isEmptyW) {
      if (b.isEmptyW) (0, 0)
      else (b.left, b.w)
    } else {
      if (b.isEmptyW) (left, w)
      else {
        val nright = math.max(right, b.right)
        val nleft = math.min(left, b.left)
        (nleft, nright - nleft)
      }
    }
    val (ntop, nh) = if (isEmptyH) {
      if (b.isEmptyH) (0, 0)
      else (b.top, b.h)
    } else {
      if (b.isEmptyH) (top, h)
      else {
        val nbottom = math.max(bottom, b.bottom)
        val ntop = math.min(top, b.top)
        (ntop, nbottom - ntop)
      }
    }
    if (b.isEmpty) this
    else if (b.w == 0)
      Rect(left, ntop, w, nh)
    else if (b.h == 0)
      Rect(nleft, top, nw, h)
    else {
      Rect(nleft, ntop, nw, nh)
    }
  }
  def growSize(v: Vector2): Rect = copy(w = w + v.x, h = h + v.y)
  def +(v: Vector2): Rect = copy(x = x + v.x, y = y + v.y)
}
