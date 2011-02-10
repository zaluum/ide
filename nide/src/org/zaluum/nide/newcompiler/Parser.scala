package org.zaluum.nide.newcompiler

import org.zaluum.nide.model.Dimension
import org.zaluum.nide.model.Point
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.JavaConversions._
object ProtoParser {
  def parse(b: BoxFileProtos.BoxClassDef,name:Option[Name]=None): BoxDef = {
    BoxDef(name.getOrElse(Name(b.getClassName)),
      image = if (b.hasImageName) Some(b.getImageName) else None,
      defs = b.getInnerClassList.map { parse(_) }.toList,
      vals = b.getInstanceList.map { parse(_) }.toList,
      ports = b.getPortList.map { parse(_) }.toList,
      connections = b.getConnectionList.map { parse(_) }.toList)
  }
  def parse(i: BoxFileProtos.BoxClassDef.Instance): ValDef = {
    // TODO parameters
    val guiPos = if (i.hasGuiPos) Some(parse(i.getGuiPos)) else None
    val guiSize = if (i.hasGuiSize) Some(parseDim(i.getGuiSize)) else None
    val size = if (i.hasSize) Some(parseDim(i.getSize)) else None
    ValDef(Name(i.getName), Name(i.getClassName), parse(i.getPos), size, guiPos, guiSize)
  }
  def parse(c: BoxFileProtos.BoxClassDef.Connection): ConnectionDef = {
    ConnectionDef(
      if (c.hasSource) parse(c.getSource) else EmptyTree,
      if (c.hasTarget) parse(c.getTarget) else EmptyTree)
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
