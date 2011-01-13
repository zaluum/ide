package org.zaluum.nide.model

import org.eclipse.draw2d.Figure
import scala.collection.mutable.Buffer
import org.zaluum.nide.protobuf.BoxFileProtos

sealed trait OrtoDirection {
  def const(p:Tuple2) : Int
  def vari(p:Tuple2) : Int
  def orto : OrtoDirection
}
object V extends OrtoDirection {
  def const(p:Tuple2) = p.x
  def vari(p:Tuple2) = p.y
  def orto = H 
}
object H extends OrtoDirection {
  def const(p:Tuple2) = p.y
  def vari(p:Tuple2) = p.x
  def orto = V
} 
case class Line(val dir:OrtoDirection, var from:Point, var len:Int) {
  def this(dir:OrtoDirection,from:Point,to:Point) = this(dir,from,dir.vari(to-from))
  def canExtendTo(to:Point) = dir.const(from) == dir.const(to)
  def extendTo(to:Point) { len = dir.vari(to-from) }
  def end = dir match {
    case H => Point(from.x+len,from.y)
    case V => Point(from.x, from.y+len)
  }
}
case class PortRef(box:Box, name:String) {
  def toProto = {
    val proto = BoxFileProtos.Contents.PortRef.newBuilder()
    proto.setPortName(name)
    proto.setBoxName(box.name)
    proto.build
  }
}
object Connection{
  def apply(model:Model, from:Box, fromP:String, to:Box, toP:String){
    val c = new Connection(Some(PortRef(from,fromP)),Some(PortRef(to,toP)))
    model.connections += c
  }
}
class Connection(var from:Option[PortRef], var to:Option[PortRef]) {
  var buf = Buffer[Line]()
  override def toString = "Connection(" + from + "->" + to+ ")"
  def toProto = {
    val proto = BoxFileProtos.Contents.Connection.newBuilder
    from foreach { port=> proto.setSource(port.toProto) }
    to foreach { port => proto.setTarget(port.toProto) }
    proto.build
    // TODO waypoints
  }
  def simpleConnect(src:Point,dst:Point) {
     buf = if (src==dst) {
      Buffer() 
    }else if (src.x == dst.x) {
      Buffer(Line(H,src, dst.x - src.x))
    }else if (src.y == dst.y){
      Buffer(Line(V,src, dst.y - src.y))
    }else {
      val despl = (dst.x-src.x)/2
      Buffer(Line(H,src,despl), Line(V,src>>despl, dst.y-src.y),Line(H,dst >> -despl, despl))
    }
  }
  
  def appendEnd(to:Point, dir:OrtoDirection) {
    val l = buf.last
    if (l.canExtendTo(to)) l.extendTo(to)
    else {
      if (l.dir == dir) {
        l.extendTo(to)
        buf += new Line(dir.orto,l.end,to)
      } else {
        val l1 =  new Line(dir,l.end,to)
        buf += l1
        buf += new Line(dir.orto,l1.end,to)
      }
    }
  }
}
