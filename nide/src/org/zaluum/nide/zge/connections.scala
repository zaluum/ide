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
}
object H extends OrtoDirection {
  def const(p: Tuple2) = p.y
  def vari(p: Tuple2) = p.x
  def orto = V
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

/*object Route {
  def apply(src:Point,dst:Point) : Route = {
    val lines = if (src == dst) {
      List()
    } else if (src.x == dst.x) {
      List(Line(V, src, dst.y - src.y))
    } else if (src.y == dst.y) {
      List(Line(H, src, dst.x - src.x))
    } else {
      val despl = (dst.x - src.x) / 2
      List(Line(H, src, despl), Line(V, src >> despl, dst.y - src.y), Line(H, dst >> -despl, despl))
    }
    Route(src,dst,lines)
  }
}*/
object Route { 
  def apply(src:Point,dst:Point) : Route = {
    val mid = Point(src.x + (dst.x-src.x)/2, dst.y)
    Route (List(src,mid,dst))
  }
}
case class Route(val points : List[Point]) {
  def ¬(src:Point,dst:Point) = {
    if (src == dst) {
      List()
    }else if (src.x == dst.x) {
      List(Line(V, src, dst.y - src.y))
    } else if (src.y == dst.y) {
      List(Line(H, src, dst.x - src.x))
    } else {
      val despl = (dst.x - src.x) 
      List(Line(H, src, despl), Line(V, src >> despl, dst.y - src.y))
    }
  }
  lazy val lines = makePath(points)
  def makePath(path:List[Point]) : List[Line] = {
    path match {
      case Nil => Nil
      case e :: Nil => Nil
      case from :: to :: tail => ¬(from,to) ++ makePath(to::tail) 
    }
  }
}
/*case class Route(val src:Point, val dst:Point, val lines : List[Line]) {
  def appendEnd(to: Point, dir: OrtoDirection) {
    val l = lines.last
    if (l.canExtendTo(to)) {
      lines.dropRight(1) ++ List(l.extendTo(to))
    }
    else {
      if (l.dir == dir) {
        lines.dropRight(1) ++ List(l.extendTo(to), Line(dir.orto, l.end, to))
      } else {
        val l1 = Line(dir, l.end, to)
        lines.dropRight(1) ++ List(
            l1,
            Line(dir.orto, l1.end, to))
      }
    }
  }
}*/
