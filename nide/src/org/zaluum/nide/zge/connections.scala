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
  def high = math.max(start,end)
  def intersect(other: Interval, nearEnd : Boolean): Option[Int] = {
    if (other.high < low || other.low > high) None // |--| <-->
    else {
      if (nearEnd) {
        if (end>start) {
          if (other.low>=start && other.high <= end) Some(other.high) // s--<--(>)--e
          else if (other.low <= end) Some(end)                        // s--<--(e)-->
          else Some(other.high)                                       // <--s--(>)--e
        }else {
          if (other.low>=end && other.high <= start) Some(other.low)  // e--(<)-->--s
          else if (other.low <= start) Some(other.low)                // e--(<)--s-->
          else Some(end)                                              // <--(e)-->--s          
        }
      }else { // near start
        if (end>start) { 
          if (other.low>=start && other.high <= end) Some(other.low)  // s--(<)-->--e
          else if (other.low <= end) Some(other.low)                  // s--(<)--e-->
          else Some(start)                                            // <--(s)-->--e
        }else {
          if (other.low>=end && other.high <= start) Some(other.high) // e--<--(>)--s
          else if (other.low <= start) Some(start)                    // e--<--(s)-->
          else Some(other.high)                                       // <--e--(>)--s          
        }
      }
    }
  }
  def contains(i: Int) = i >= low && i <= high
}
case class Line(val from: Waypoint, val to: Waypoint, primary: Boolean) {
  def midPoint = if (from.d == V) Point(from.x, to.y) else Point(to.x, from.y)
  def start = if (primary) from.p else midPoint
  def end = if (primary) midPoint else to.p
  def dir = if (primary) from.d else from.d.orto
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

  def intersect(other: Line, nearEnd : Boolean): Option[Point] = {
    (dir, other.dir) match {
      case (H, H) ⇒
        if (start.y != other.start.y) None
        else intervalX.intersect(other.intervalX, nearEnd) map (x ⇒ Point(x, start.y))
      case (V, V) ⇒
        if (start.x != other.start.x) None
        else intervalY.intersect(other.intervalY, nearEnd) map (y ⇒ Point(start.x, y))
      case (H, V) ⇒
        if (intervalX.contains(other.start.x) && other.intervalY.contains(start.y))
          Some(Point(other.start.x, start.y))
        else None
      case (V, H) ⇒
        if (intervalY.contains(other.start.y) && other.intervalX.contains(start.x))
          Some(Point(start.x, other.start.y))
        else None
    }
  }
}
/*case class Line(val dir: OrtoDirection, val from: Point, val len: Int) {
  def canExtendTo(to: Point) = dir.const(from) == dir.const(to)
  def extendTo(to: Point) = { copy(len = dir.vari(to - from)) }
  def end = dir match {
    case H ⇒ Point(from.x + len, from.y)
    case V ⇒ Point(from.x, from.y + len)
  }
}*/

/*// routes should be connected by their ends
case class Graph(routes:List[Route]) {
  lazy val jointPoints = {
    var map = Map[Point,Set[Route]]()
    for (r<-routes) {
      val p = wp.p
      if (map.contains(p))
        map += (p -> (map(p) +r))
      else 
        map += (p -> Set(r))
      map.filterNot {e => e._2.size==1 }
    }
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
  private def intersection(remaining: List[Line], nearEnd:Boolean,others: List[ConnectionDef], toPoint: (Line ⇒ Point)): Option[(ConnectionDef,Line, Point)] =
    remaining match {
      case Nil ⇒ None
      case h :: tail ⇒
        
        val intersections: List[(ConnectionDef,Point)] = for {
          cd ← others
          ol ← cd.route.lines
          p ← ol.intersect(h,!nearEnd)
        } yield (cd,p)
        if (intersections.isEmpty) {
          intersection(tail, nearEnd, others, toPoint)
        } else {
          val (cd,p) = intersections.reduceLeft{
            (a,b) ⇒ 
            val (ra,ap) = a
            val (rb,bp) = b
            if (ap.distanceOrto(toPoint(h)) < bp.distanceOrto(toPoint(h))) a else b
          }
          Some((cd,h, p))
        }
    }

  def lastIntersection(others: List[ConnectionDef]): Option[(ConnectionDef,Line, Point)] = {
    intersection(lines, false, others, _.end)
  }
  def firstIntersection(others: List[ConnectionDef]): Option[(ConnectionDef, Line, Point)] = {
    intersection(lines.reverse, true, others, _.start)
  }
  def liesIn(from: Waypoint, mid: Waypoint, to: Waypoint) = {
    if (from.d == V) {
      (mid.d == V && mid.x == from.x) || (mid.y == to.y)
    } else {
      (mid.d == H && mid.y == from.y) || (mid.x == to.x)
    }
  }
  // p has to lie in some segment of the route
  def split(p: Point): (Route, Route) = {
    val seg = lines.find(_.contains(p)).get
    val after = points.takeWhile(_ != seg.to)
    val before = points.dropWhile(_ != seg.from).drop(1)
    val wp = Waypoint(p,H)
    val fbefore = if (seg.from.p != p) Route(Waypoint(p,H) :: seg.from :: before)
      else Route(Waypoint(p,H) :: before)
    val fafter = if (seg.to.p!=p ) Route(after ::: seg.to :: Waypoint(p,H) :: Nil) 
      else Route(after ::: Waypoint(p,H) :: Nil)
    (fbefore,fafter)
  }
  def extend(to: Waypoint, dir: OrtoDirection): Route = {
    points match {
      case h :: Nil ⇒ // h is H
        if (dir == H) { // make an N
          val mid = Point(h.x + (to.x - h.x) / 2, to.y)
          extend(Waypoint(mid, H)).extend(to)
        } else {
          extend(to)
        }
      case h :: tail ⇒
        Route(tail).extend(Waypoint(h.p, dir.orto)).extend(to)
      case _ ⇒ extend(to)
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
  def ¬(src: Waypoint, dst: Waypoint) = {
    if (src.p == dst.p) {
      List()
    } else {
      val despl = dst.x - src.x
      List(Line(src, dst, true), Line(src, dst, false))
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
