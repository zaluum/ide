package org.zaluum.nide.compiler

import org.zaluum.nide.zge.Clipboard
import org.zaluum.nide.zge.V
import org.zaluum.nide.zge.H
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.JavaConversions._
import java.io.InputStream
import java.nio.charset.Charset
import java.io.ByteArrayInputStream
object Parser {
  def readTree(i: InputStream, className: Name) : BoxDef= {
    val a = try {
      val proto = BoxFileProtos.BoxClassDef.parseFrom(i)
      parse(proto, Some(className))
    } catch {
      case e ⇒
        // TODO fixme better handling
        //e.printStackTrace;
        println("PARSING ERROR! " +e )
        BoxDef(Name(""), Name(""), None, guiSize = Some(Dimension(250, 250)), None, List(), List(), List(), List(), List())
    }
    a.assignLine(1)
    a
  }
  def readTree(isoString:String, className: Name) : BoxDef = {
    val byteContents = isoString.getBytes(Charset.forName("ISO-8859-1")) // TODO ??
    readTree(new ByteArrayInputStream(byteContents),className)
  }
  def parse(b: BoxFileProtos.BoxClassDef, name: Option[Name] = None): BoxDef = {
    BoxDef(name.getOrElse(Name(b.getClassName)),
      pkg = if (b.hasPackage) Name(b.getPackage) else Name(""),
      superName = if (b.hasSuper) Some(Name(b.getSuper)) else None,
      guiSize = if (b.hasGuiSize) Some(parseDim(b.getGuiSize)) else None,
      image = if (b.hasImageName) Some(b.getImageName) else None,
      defs = b.getInnerClassList.map { parse(_) }.toList,
      vals = b.getInstanceList.map { parse(_) }.toList,
      ports = b.getPortList.map { parse(_) }.toList,
      connections = b.getConnectionList.map { parse(_) }.toList,
      junctions = b.getJunctionList.map { parse(_) }.toList)
  }
  def parse(r: BoxFileProtos.Junction): Junction = {
    Junction(Name(r.getName), parse(r.getPos))
  }
  def parse(l: BoxFileProtos.Label): LabelDesc =
    LabelDesc(l.getDescription, parse(l.getPos).toVector)
  def parse(i: BoxFileProtos.Instance): ValDef = {
    val guiPos = if (i.hasGuiPos) Some(parse(i.getGuiPos)) else None
    val guiSize = if (i.hasGuiSize) Some(parseDim(i.getGuiSize)) else None
    val size = if (i.hasSize) Some(parseDim(i.getSize)) else None

    val lbl = if (i.hasLabel) Some(parse(i.getLabel)) else None
    val lblgui = if (i.hasLabelGui) Some(parse(i.getLabelGui)) else None
    ValDef(
      Name(i.getName),
      Name(i.getClassName),
      parse(i.getPos),
      size,
      guiPos,
      guiSize,
      i.getParameterList.map { parse(_) }.toList,
      i.getConstructorParameterList.toList,
      i.getConstructorTypesList.map { Name(_) }.toList,
      lbl,
      lblgui)
  }
  def parse(p: BoxFileProtos.Parameter): Param = {
    Param(Name(p.getKey), p.getValue)
  }
  def parse(c: BoxFileProtos.Connection): ConnectionDef = {
    ConnectionDef(
      parse(c.getSource),
      parse(c.getTarget),
      c.getPointList map { parse(_) } toList)
  }
  def parse(ref: BoxFileProtos.Ref): Tree = {
    if (ref.hasPort) parse(ref.getPort)
    else if (ref.hasJunction) parse(ref.getJunction)
    else EmptyTree
  }
  def parse(jref: BoxFileProtos.JunctionRef): JunctionRef = {
    JunctionRef(Name(jref.getName))
  }
  def parse(p: BoxFileProtos.PortRef): PortRef = {
    PortRef(if (p.hasBoxName) ValRef(Name(p.getBoxName)) else ThisRef(),
      Name(p.getPortName), if (p.hasIn) p.getIn else true)
  }
  def parse(p: BoxFileProtos.Port): PortDef = {
    PortDef(name = Name(p.getName), typeName = Name(p.getType),
      dir = parseDir(p.getDirection),
      inPos = parse(p.getPosInternal),
      extPos = parse(p.getPosExternal))
  }
  def parseDir(dir: BoxFileProtos.Direction) = dir match {
    case BoxFileProtos.Direction.IN ⇒ In
    case BoxFileProtos.Direction.OUT ⇒ Out
    case BoxFileProtos.Direction.SHIFT ⇒ Shift
  }
  def parse(p: BoxFileProtos.Point) = Point(p.getX, p.getY)
  def parseDim(p: BoxFileProtos.Point) = Dimension(p.getX, p.getY)
  def parse(p: BoxFileProtos.Clipboard): Clipboard = {
    val boxes = p.getBoxClassList map { parse(_) } toList
    val vals = p.getInstanceList map { parse(_) } toList
    val ports = p.getPortList map { parse(_) } toList
    val connections = p.getConnnectionList map { parse(_) } toList;
    Clipboard(boxes, vals, ports, connections)
  }
}
