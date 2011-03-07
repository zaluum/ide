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
object Line {
  def apply(dir:OrtoDirection,from:Point,to:Point):Line = {
    Line(dir, from, dir.vari(to - from))
  }
}
case class Line(val dir: OrtoDirection, val from: Point, val len: Int) {
  def canExtendTo(to: Point) = dir.const(from) == dir.const(to)
  def extendTo(to: Point) = { copy (len = dir.vari(to - from)) }
  def end = dir match {
    case H ⇒ Point(from.x + len, from.y)
    case V ⇒ Point(from.x, from.y + len)
  }
}
object Waypoint {
  def apply(x:Int,y:Int,d:OrtoDirection) : Waypoint = Waypoint(Point(x,y),d)
}
case class Waypoint(p:Point,d:OrtoDirection)  {
  def y = p.y 
  def x = p.x
}
case class Route(points : List[Waypoint]) {
  def contains(p1:Int,p2:Int, x: Int) = {
    if (p1>p2) {
      x <= p1 && x >= p2
    } else 
      x >= p1 && x <= p2
  }
  def liesIn (from:Waypoint, mid:Waypoint, to:Waypoint) = {
    if (from.d == V) {
      (mid.d == V && mid.x==from.x && contains(from.y, to.y, mid.y)) ||
      (mid.y == to.y && contains(from.x, to.x, mid.x))
    } else {
      (mid.d == H && mid.y==from.y && contains(from.x, to.x, mid.x)) || 
      (mid.x==to.x && contains(from.y,to.y, mid.y))
    }
  }
  def extend(p:Waypoint) : Route = {
    val res = points match {
      case Nil => Route(p :: Nil)
      case h :: Nil => Route(p :: h :: Nil)
      case h :: hh :: tail =>
        if (liesIn(hh,h,p)) {// we can suppress h if it lies in the L from hh to p 
          println("can suppress")
          Route(p :: hh :: tail)
        }else { // we make the longest L possible from hh to h
          if (hh.d == V && h.d == H) {
            Route (p :: Waypoint(p.x,h.y,V) :: hh :: tail)
          } else if (hh.d == H && h.d == V) {
            Route (p :: Waypoint(h.x,p.y,H) :: hh :: tail)
          } else {
            if ( h == H && h.y == hh.y) {
              
            } 
            Route (p :: h :: hh :: tail)
          }
        }
    }
    //println ("extended to " + res)
    res
  }
  def changeHead(dir:OrtoDirection) = {
    points match {
      case Nil=> this
      case h :: Nil => this
      case h :: hh :: tail => Route(Waypoint(h.p,dir) :: hh :: tail)
    }
  }
  def head = points.head.p
  def close(p:Point) : Route = {
    points match {
      case h :: Nil => 
        val mid = Point(h.x+(p.x-h.x)/2, p.y)
        Route(Waypoint(p,H) :: Waypoint(mid,V) :: h :: Nil)
      case _ => extend(Waypoint(p,H))
    }
  }
  def ¬(src:Waypoint,dst:Waypoint) = {
    if (src.p == dst.p) {
      List()
    }/*else if (src.x == dst.x) {
      List(Line(V, src.p, dst.y - src.y))
    } else if (src.y == dst.p.y) {
      List(Line(H, src.p, dst.x - src.x))
    } */else if (src.d == H){
      val despl = dst.x - src.x 
      List(Line(V, src.p >> despl, dst.y - src.y),Line(H, src.p, despl))
    } else  { // V
      val despl = dst.y - src.p.y
      List(Line(H, src.p ^ despl, dst.x-src.x),Line(V,src.p,despl))
    }
  }
  lazy val lines = makePath(points)
  def makePath(path:List[Waypoint]) : List[Line] = {
    path match {
      case Nil => Nil
      case e :: Nil => Nil
      case to :: from :: tail => ¬(from,to) ++ makePath(from::tail) 
    }
  }
}
