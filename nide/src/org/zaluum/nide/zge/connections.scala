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

object Route {
  def apply(src:Point,dst:Point) : Route = {
    val lines = if (src == dst) {
      List()
    } else if (src.x == dst.x) {
      List(Line(H, src, dst.x - src.x))
    } else if (src.y == dst.y) {
      List(Line(V, src, dst.y - src.y))
    } else {
      val despl = (dst.x - src.x) / 2
      List(Line(H, src, despl), Line(V, src >> despl, dst.y - src.y), Line(H, dst >> -despl, despl))
    }
    Route(src,dst,lines)
  }
}
case class Route(val src:Point, val dst:Point, val lines : List[Line]) {
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
}
 /* def connectionFlow(typeMap: Map[PortRef, TypedPort]): Option[(PortRef, PortRef)] =
    (from, to) match {
      case (Some(p1), Some(p2)) ⇒
        (p1, p2, typeMap(p1).in, typeMap(p2).in) match {
          case (m1: ModelPortRef, m2: ModelPortRef, false, true) ⇒ Some((m2, m1))
          case (m1: ModelPortRef, m2: ModelPortRef, true, false) ⇒ Some((m1, m2))
          case (m1: ModelPortRef, b2: BoxPortRef, true, true) ⇒ Some((m1, b2))
          case (m1: ModelPortRef, b2: BoxPortRef, false, false) ⇒ Some((b2, m1))
          case (b1: BoxPortRef, m2: ModelPortRef, true, true) ⇒ Some((m2, b1))
          case (b1: BoxPortRef, m2: ModelPortRef, false, false) ⇒ Some((b1, m2))
          case (b1: BoxPortRef, b2: BoxPortRef, true, false) if (b1.box != b2.box) ⇒ Some((b2, b1))
          case (b1: BoxPortRef, b2: BoxPortRef, false, true) if (b1.box != b2.box) ⇒ Some((b1, b2))
          case _ ⇒ None
        }
      case _ ⇒ None
    }*/
