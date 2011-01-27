package org.zaluum.nide.model
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.mutable.Buffer

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
case class Line(val dir: OrtoDirection, var from: Point, var len: Int) {
  def this(dir: OrtoDirection, from: Point, to: Point) = this(dir, from, dir.vari(to - from))
  def canExtendTo(to: Point) = dir.const(from) == dir.const(to)
  def extendTo(to: Point) { len = dir.vari(to - from) }
  def end = dir match {
    case H ⇒ Point(from.x + len, from.y)
    case V ⇒ Point(from.x, from.y + len)
  }
}
sealed trait PortRef extends Locatable {
  val name: String
  def protoAttach(proto: BoxFileProtos.Contents.PortRef.Builder): Unit
  def toProto = {
    val proto = BoxFileProtos.Contents.PortRef.newBuilder()
    proto.setPortName(name)
    protoAttach(proto)
    proto.build
  }
}
sealed case class BoxPortRef(val box: Box, val name: String) extends PortRef {
  def protoAttach(proto: BoxFileProtos.Contents.PortRef.Builder) {
    proto.setBoxName(box.name)
  }
  def location = Location("$" + box.location.str + "#" + name)
}
sealed case class ModelPortRef(val name: String) extends PortRef {
  def protoAttach(proto: BoxFileProtos.Contents.PortRef.Builder) {}
  def location = Location("$#" + name)
}

object Connection {
  def apply(bcd: BoxClassDecl, from: Box, fromP: String, to: Box, toP: String) {
    val c = new Connection(Some(BoxPortRef(from, fromP)), Some(BoxPortRef(to, toP)))
    bcd.connections += c
  }
  def apply(bcd: BoxClassDecl, from: PortDecl, to: Box, toP: String) {
    val c = new Connection(Some(ModelPortRef(from.name)), Some(BoxPortRef(to, toP)))
    bcd.connections += c
  }
  def apply(bcd: BoxClassDecl, from: Box, fromP: String, to: PortDecl) {
    val c = new Connection(Some(BoxPortRef(from, fromP)), Some(ModelPortRef(to.name)))
    bcd.connections += c
  }
  def apply(bcd: BoxClassDecl, from: PortDecl, to: PortDecl) {
    val c = new Connection(Some(ModelPortRef(from.name)), Some(ModelPortRef(to.name)))
    bcd.connections += c
  }

}
class Connection(var from: Option[PortRef], var to: Option[PortRef]) extends Locatable {
  var buf = Buffer[Line]()
  def location = Location("(" + from.map(_.location.str).getOrElse("") + "," + to.map(_.location.str).getOrElse("") + ")")
  override def toString = "Connection(" + from + "->" + to + ")"
  def toProto = {
    val proto = BoxFileProtos.Contents.Connection.newBuilder
    from foreach { port ⇒ proto.setSource(port.toProto) }
    to foreach { port ⇒ proto.setTarget(port.toProto) }
    proto.build
    // TODO waypoints
  }
  def connectionFlow(typeMap: Map[PortRef, TypedPort]): Option[(PortRef, PortRef)] =
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
    }
  def simpleConnect(src: Point, dst: Point) {
    buf = if (src == dst) {
      Buffer()
    } else if (src.x == dst.x) {
      Buffer(Line(H, src, dst.x - src.x))
    } else if (src.y == dst.y) {
      Buffer(Line(V, src, dst.y - src.y))
    } else {
      val despl = (dst.x - src.x) / 2
      Buffer(Line(H, src, despl), Line(V, src >> despl, dst.y - src.y), Line(H, dst >> -despl, despl))
    }
  }

  def appendEnd(to: Point, dir: OrtoDirection) {
    val l = buf.last
    if (l.canExtendTo(to)) l.extendTo(to)
    else {
      if (l.dir == dir) {
        l.extendTo(to)
        buf += new Line(dir.orto, l.end, to)
      } else {
        val l1 = new Line(dir, l.end, to)
        buf += l1
        buf += new Line(dir.orto, l1.end, to)
      }
    }
  }
}
