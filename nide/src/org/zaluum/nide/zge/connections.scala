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
case class Waypoint(p:Point,d:OrtoDirection) {
  def y = p.y 
  def x = p.x
}
case class Route(points : List[Waypoint]) {
  def extend(p:Point) : Route = {
    val res = points match {
      case Nil => Route(Waypoint(p,H) :: Nil)
      case h :: Nil => Route(Waypoint(p,h.d.orto) :: h :: Nil)
      case h :: tail =>  Route( Waypoint(p,h.d.orto) :: (
              if (h.d==V) 
                Waypoint(Point(h.x,p.y),H) 
              else
                Waypoint(Point(p.x,h.y),V)
            ) :: tail
            )
             
    }
    //println ("extended to " + res)
    res
  }
  def ¬(src:Waypoint,dst:Waypoint) = {
    if (src.p == dst.p) {
      List()
    }else if (src.x == dst.x) {
      List(Line(V, src.p, dst.y - src.y))
    } else if (src.y == dst.p.y) {
      List(Line(H, src.p, dst.x - src.x))
    } else if (src.d == H){
      val despl = dst.x - src.x 
      List(Line(H, src.p, despl), Line(V, src.p >> despl, dst.y - src.y))
    } else  { // V
      val despl = dst.y - src.p.y
      List(Line(V,src.p,despl), Line(H, src.p ^ despl, dst.x-src.x))
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
