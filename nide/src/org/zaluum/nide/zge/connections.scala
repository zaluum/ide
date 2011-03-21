package org.zaluum.nide.zge

import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.mutable.Buffer
import org.zaluum.nide.compiler._

sealed trait OrtoDirection {
  def const(p: Tuple2): Int
  def vari(p: Tuple2): Int
  def orto: OrtoDirection
}
object V extends OrtoDirection {
  def const(p: Tuple2) = p.x
  def vari(p: Tuple2) = p.y
  def orto = H
  override def toString = "V"
}
object H extends OrtoDirection {
  def const(p: Tuple2) = p.y
  def vari(p: Tuple2) = p.x
  def orto = V
  override def toString = "H"
}
case class Interval(start: Int, end: Int) {
  def low = math.min(start,end)
  def isZero = start==end
  def high = math.max(start,end)
  def intersect(other: Interval, nearEnd : Boolean): List[Int] = {
    if (other.high < low || other.low > high) List() // |--| <-->
    else {
      if (nearEnd) {
        if (end>start) {
          if (other.low>=start && other.high <= end) List(other.high,other.low) // s--<--(>)--e
          else if (other.low <= end) List(end,other.low)                        // s--<--(e)-->
          else List(other.high, start)                                          // <--s--(>)--e
        }else {
          if (other.low>=end && other.high <= start) List(other.low, other.high)  // e--(<)-->--s
          else if (other.low <= start) List(other.low,start)                      // e--(<)--s-->
          else List(end, other.high)                                              // <--(e)-->--s          
        }
      }else { // near start
        if (end>start) { 
          if (other.low>=start && other.high <= end) List(other.low, other.high) // s--(<)-->--e
          else if (other.low <= end) List(other.low,end)                         // s--(<)--e-->
          else List(start,other.high)                                            // <--(s)-->--e
        }else {
          if (other.low>=end && other.high <= start) List(other.high, other.low)// e--<--(>)--s
          else if (other.low <= start) List(start,other.low)                    // e--<--(s)-->
          else List(other.high,end)                                             // <--e--(>)--s          
        }
      }
    }
  }
  def contains(i: Int) = i >= low && i <= high
}
case class Line(val from: Point, val to: Point, primary: Boolean) {
  def midPoint = Point(to.x, from.y)
  def start = if (primary) from else midPoint
  def end = if (primary) midPoint else to
  def dir = if (primary) H else V
  override def toString = "("+start+","+end+")"
  def project(p: Point): Point = {
    val a = p - start
    val b = end - start
    val ab = a dot b
    val bb = b dot b
    val div = ab / bb
    val v = b * div
    start + v
  }
  def distance(p: Point): Double = {
    java.awt.geom.Line2D.ptSegDist(start.x, start.y, end.x, end.y, p.x, p.y)
  }
  def contains(p: Point): Boolean = {
    distance(p) < 0.001
  }
  import math.{ min, max }
  def intervalX = Interval(start.x, end.x)
  def intervalY = Interval(start.y, end.y)

  def intersect(other: Line, nearEnd : Boolean = false): List[Point] = {
    (dir, other.dir) match {
      case (H, H) ⇒
        if (start.y != other.start.y || intervalX.isZero || other.intervalX.isZero) List()
        else intervalX.intersect(other.intervalX,nearEnd) map (x ⇒ Point(x, start.y))
      case (V, V) ⇒
        if (start.x != other.start.x || intervalY.isZero || other.intervalY.isZero) List()
        else {
          val p = intervalY.intersect(other.intervalY,nearEnd) map (y ⇒ Point(start.x, y))
          p
        }
      case (H, V) ⇒
        if (!intervalX.isZero && !other.intervalY.isZero && intervalX.contains(other.start.x) && other.intervalY.contains(start.y))
          List(Point(other.start.x, start.y))
        else List()
      case (V, H) ⇒
        if (!intervalY.isZero && ! other.intervalX.isZero && intervalY.contains(other.start.y) && other.intervalX.contains(start.x))
          List(Point(start.x, other.start.y))
        else List()
    }
  }
}

object Waypoint {
  def apply(x: Int, y: Int, d: OrtoDirection): Waypoint = Waypoint(Point(x, y), d)
}
case class Waypoint(p: Point, d: OrtoDirection) {
  def y = p.y
  def x = p.x
}
