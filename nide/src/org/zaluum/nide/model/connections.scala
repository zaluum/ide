package org.zaluum.nide.model

import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.Figure
import scala.collection.mutable.Buffer

object P {
}
case class P(var x:Int, var y:Int) {
  def >> (i:Int) = P(x+i,y)
  def ^ (i:Int) = P(x,y+i)
  def - (to:P) = P(to.x-x,to.y-y)
  def + (to:P) = P(to.x+x,to.y+y)
}

sealed trait OrtoDirection {
  def const(p:P) : Int
  def vari(p:P) : Int
  def orto : OrtoDirection
}
object V extends OrtoDirection {
  def const(p:P) = p.x
  def vari(p:P) = p.y
  def orto = H 
}
object H extends OrtoDirection {
  def const(p:P) = p.y
  def vari(p:P) = p.x
  def orto = V
}
case class Line(val dir:OrtoDirection, var from:P, var len:Int) {
  def this(dir:OrtoDirection,from:P,to:P) = this(dir,from,dir.vari(to-from))
  def canExtendTo(to:P) = dir.const(from) == dir.const(to)
  def extendTo(to:P) { len = dir.vari(to-from) }
  def end = dir match {
    case H => P(from.x+len,from.y)
    case V => P(from.x, from.y+len)
  }
}

class Connection(var from:Option[Port], var to:Option[Port]) {
  var buf = Buffer[Line]()
  def simpleConnect(src:P,dst:P) {
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
  
  def appendEnd(to:P, dir:OrtoDirection) {
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
