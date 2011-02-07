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
    val guitree= if (i.hasGuiPos && i.hasGuiSize) 
        SizeDef(parse(i.getGuiPos),parseDim(i.getGuiSize)) 
      else
        EmptyTree  
    ValDef(Name(i.getName), Name(i.getClassName), parse(i.getPos), guitree)
  }
  def parse(c: BoxFileProtos.BoxClassDef.Connection): ConnectionDef = {
    ConnectionDef(
      if (c.hasSource) parse(c.getSource) else EmptyTree,
      if (c.hasTarget) parse(c.getTarget) else EmptyTree)
  }
  def parse(p: BoxFileProtos.BoxClassDef.PortRef): PortRef = {
    PortRef(if (p.hasBoxName) ValRef(Name(p.getBoxName)) else ThisRef,
      Name(p.getPortName))
  }
  def parse(p: BoxFileProtos.BoxClassDef.Port): PortDef = {
    PortDef(name = Name(p.getName), typeName = Name(p.getType),
      in = (p.getDirection == BoxFileProtos.BoxClassDef.Direction.IN),
      inPos = parse(p.getPosInternal),
      extPos = parse(p.getPosExternal))
  }
  def parse(p: BoxFileProtos.BoxClassDef.Point) = Point(p.getX, p.getY)
  def parseDim(p: BoxFileProtos.BoxClassDef.Point) = Dimension(p.getX,p.getY)
}
