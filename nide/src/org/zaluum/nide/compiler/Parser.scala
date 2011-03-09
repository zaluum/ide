package org.zaluum.nide.compiler

import org.zaluum.nide.zge.V
import org.zaluum.nide.zge.H
import org.zaluum.nide.zge.Waypoint
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.JavaConversions._
object Parser {
  def parse(b: BoxFileProtos.BoxClassDef,name:Option[Name]=None): BoxDef = {
    BoxDef(name.getOrElse(Name(b.getClassName)), 
      superName = if (b.hasSuper) Some(Name(b.getSuper)) else None,
      image = if (b.hasImageName) Some(b.getImageName) else None,
      defs = b.getInnerClassList.map { parse(_) }.toList,
      vals = b.getInstanceList.map { parse(_) }.toList,
      ports = b.getPortList.map { parse(_) }.toList,
      connections = b.getConnectionList.map { parse(_) }.toList,
      junctions = b.getJunctionList.map {parse(_)}.toList)
  }
  def parse(r: BoxFileProtos.BoxClassDef.Junction) : Junction = {
    Junction(Name(r.getName), parse(r.getPos))
  }
  def parse(i: BoxFileProtos.BoxClassDef.Instance): ValDef = {
    val guiPos = if (i.hasGuiPos) Some(parse(i.getGuiPos)) else None
    val guiSize = if (i.hasGuiSize) Some(parseDim(i.getGuiSize)) else None
    val size = if (i.hasSize) Some(parseDim(i.getSize)) else None
    ValDef(
        Name(i.getName), 
        Name(i.getClassName), 
        parse(i.getPos), 
        size, 
        guiPos, 
        guiSize,    
        i.getParameterList.map { parse(_)}.toList)
  }
  def parse(p:BoxFileProtos.BoxClassDef.Parameter) : Param = {
    Param(Name(p.getKey),p.getValue)
  }
  def parse(c: BoxFileProtos.BoxClassDef.Connection): ConnectionDef = {
    ConnectionDef(
      parse(c.getSource),
      parse(c.getTarget),
      c.getWaypointList map { parse(_)} toList)
  }
  def parse(ref : BoxFileProtos.BoxClassDef.Ref) : Tree = {
    if (ref.hasPort) parse(ref.getPort) 
    else if (ref.hasJunction) parse(ref.getJunction)
    else EmptyTree
  }
  def parse(jref : BoxFileProtos.BoxClassDef.JunctionRef) : JunctionRef = {
    JunctionRef(Name(jref.getName))
  }
  def parse(w: BoxFileProtos.BoxClassDef.Waypoint) : Waypoint = {
    Waypoint(parse(w.getPoint), if (w.getDir == BoxFileProtos.BoxClassDef.Orto.H) H else V)
  }
  def parse(p: BoxFileProtos.BoxClassDef.PortRef): PortRef = {
    PortRef(if (p.hasBoxName) ValRef(Name(p.getBoxName)) else ThisRef,
      Name(p.getPortName),if (p.hasIn) p.getIn else true)
  }
  def parse(p: BoxFileProtos.BoxClassDef.Port): PortDef = {
    PortDef(name = Name(p.getName), typeName = Name(p.getType),
      dir = parseDir(p.getDirection),
      inPos = parse(p.getPosInternal),
      extPos = parse(p.getPosExternal))
  }
  def parseDir (dir : BoxFileProtos.BoxClassDef.Direction) = dir match {
    case BoxFileProtos.BoxClassDef.Direction.IN => In
    case BoxFileProtos.BoxClassDef.Direction.OUT => Out
    case BoxFileProtos.BoxClassDef.Direction.SHIFT => Shift   
  }
  def parse(p: BoxFileProtos.BoxClassDef.Point) = Point(p.getX, p.getY)
  def parseDim(p: BoxFileProtos.BoxClassDef.Point) = Dimension(p.getX,p.getY)
}
