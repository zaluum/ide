package graystone.zaluum.interpreter

import scala.collection.mutable.HashMap
import scala.collection.mutable.MultiMap
import graystone.zaluum.PortM
import graystone.zaluum.ConnectionBoxM
import graystone.zaluum.BoxM
class InterpreterCompiler(reporter: Reporter) {
  
  def compile(b: BoxM) : Box = b match {
    case c: ConnectionBoxM => compileConnection(c)
    case i: BoxM => compileInstance(i)
  }
  private def valueFromStr(str: Option[String], cl: Class[_]) = {
    null
  }
  def createPort(p: PortM, b:Box) : Port= {
    try {
      val cl = Class.forName(p.typ)
      val v = valueFromStr(p.default, cl)
      val port = new SimplePort(p.name, v)
      port.box = b
      port
    } catch {
      case c: ClassNotFoundException =>
        reporter += c.toString
        reporter.cancel
    }
  }
  
  private def compileConnection(cm: ConnectionBoxM) = {
    val boxMap = (cm.boxes map (bm => (bm -> compile(bm)))).toMap
    reporter.check
    val c = new IConnectionBox(cm.name)
    cm.inputs foreach { c.inputs += createPort(_,c) }
    cm.outputs foreach { c.outputs += createPort(_,c) }
    def findPort(pm: PortM, ports : Seq[Port]) = 
        ports find { _.name == pm.name } getOrElse {reporter += "port not found"; reporter.cancel }
    def boxOf (pm:PortM) = if (pm.box == c) c else boxMap(pm.box)
    def findIn(pm:PortM, box:Box) = findPort(pm,box.inputs)    
    def findOut(pm:PortM, box: Box) = findPort(pm,box.outputs)
    
    for ((from, to) <- cm.connections) {
      if (from.typ != to.typ) 
        reporter += "incompatible types"
      else
      {
        val fromB = boxOf(from)
        val toB = boxOf(to)
        c.connections.addBinding(
            if (fromB == c) findIn(from,fromB) else findOut(to,toB),
            if (toB == c) findOut(from,fromB) else findIn(to,toB))
      }
    }
    c
  }
  private def compileInstance(i: BoxM) = 
    new IInstanceBox(i.name, Class.forName(i.className).newInstance.asInstanceOf[SimpleFunc])

}