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
/*object Line {
  def apply(dir: OrtoDirection, from: Point, to: Point): Line = {
    Line(dir, from, dir.vari(to - from))
  }
}*/
case class Line(val from: Waypoint, val to: Waypoint, primary: Boolean) {
  def midPoint = if (from.d == V) Point(from.x, to.y) else Point(to.x, from.y)
  def start = if (primary) from.p  else midPoint
  def end = if (primary) midPoint else to.p
}
/*case class Line(val dir: OrtoDirection, val from: Point, val len: Int) {
  def canExtendTo(to: Point) = dir.const(from) == dir.const(to)
  def extendTo(to: Point) = { copy(len = dir.vari(to - from)) }
  def end = dir match {
    case H ⇒ Point(from.x + len, from.y)
    case V ⇒ Point(from.x, from.y + len)
  }
}*/
object Waypoint {
  def apply(x: Int, y: Int, d: OrtoDirection): Waypoint = Waypoint(Point(x, y), d)
}
case class Waypoint(p: Point, d: OrtoDirection) {
  def y = p.y
  def x = p.x
}
case class Route(points: List[Waypoint]) {
  def liesIn(from: Waypoint, mid: Waypoint, to: Waypoint) = {
    if (from.d == V) {
      (mid.d == V && mid.x == from.x) || (mid.y == to.y)
    } else {
      (mid.d == H && mid.y == from.y) || (mid.x == to.x)
    }
  }
  def extend(to: Waypoint): Route = {
    val res = points match {
      case Nil ⇒ Route(to :: Nil)
      case from :: Nil ⇒ Route(to :: from :: Nil)
      case mid :: from :: tail ⇒
        if (liesIn(from, mid, to)) { // we can suppress h if it lies in the L from hh to p 
          Route(to :: from :: tail)
        } else { // we make the longest L possible 
          (from.d, mid.d) match {
            case (V, H) ⇒
              Route(to :: Waypoint(to.x, mid.y, V) :: from :: tail)
            case (H, V) ⇒
              Route(to :: Waypoint(mid.x, to.y, H) :: from :: tail)
            case _ ⇒
              Route(to :: mid :: from :: tail)
          }
        }
    }
    //println ("extended to " + res)
    res
  }
  def changeHead(dir: OrtoDirection) = {
    points match {
      case Nil ⇒ this
      case h :: Nil ⇒ this
      case h :: hh :: tail ⇒ Route(Waypoint(h.p, dir) :: hh :: tail)
    }
  }
  def head = points.head.p
  def close(p: Point): Route = {
    points match {
      case h :: Nil ⇒
        val mid = Point(h.x + (p.x - h.x) / 2, p.y)
        Route(Waypoint(p, H) :: Waypoint(mid, V) :: h :: Nil)
      case _ ⇒ extend(Waypoint(p, H))
    }
  }
  def ¬(src: Waypoint, dst: Waypoint) = {
    if (src.p == dst.p) {
      List()
    } /*else if (src.x == dst.x) {
      List(Line(V, src.p, dst.y - src.y))
    } else if (src.y == dst.p.y) {
      List(Line(H, src.p, dst.x - src.x))
    } */ else {
      val despl = dst.x - src.x
      List(Line(src,dst,true), Line(src,dst,false))
    }
  }
  lazy val lines = makePath(points)
  def makePath(path: List[Waypoint]): List[Line] = {
    path match {
      case Nil ⇒ Nil
      case e :: Nil ⇒ Nil
      case to :: from :: tail ⇒ ¬(from, to) ++ makePath(from :: tail)
    }
  }
}
