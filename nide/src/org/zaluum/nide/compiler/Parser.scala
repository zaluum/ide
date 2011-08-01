package org.zaluum.nide.compiler

import org.zaluum.nide.zge.V
import org.zaluum.nide.zge.H
import org.zaluum.nide.protobuf.ZaluumProtobuf
import scala.collection.JavaConversions._
import java.io.InputStream
import java.nio.charset.Charset
import java.io.ByteArrayInputStream
import com.google.protobuf.ExtensionRegistry
import org.zaluum.nide.NotImplemented._
object Parser {
  def readTree(i: InputStream, className: Name): BoxDef = {
    val a = try {
      val registry = ExtensionRegistry.newInstance();
      registry.add(ZaluumProtobuf.ClassInstance.instance)
      registry.add(ZaluumProtobuf.StatInstance.instance)
      val proto = ZaluumProtobuf.BoxClass.parseFrom(i, registry)
      parse(proto, Some(className))
    } catch {
      case e ⇒
        // TODO fixme better handling
        //e.printStackTrace;
        println("PARSING ERROR! " + e)
        emptyBox
    }
    a.assignLine(1)
    a
  }
  def emptyBox = {
    val block = Block(junctions = List(),
      connections = List(),
      parameters = List(),
      valDefs = List())
    val template = Template(ports = List(), blocks = List(block))
    BoxDef(name = Name(""),
      pkg = Name(""),
      superName = None,
      guiSize = Some(Dimension(250, 250)),
      image = None,
      template = template)
  }
  def readTree(isoString: String, className: Name): BoxDef = {
    val byteContents = isoString.getBytes(Charset.forName("ISO-8859-1")) // TODO ??
    readTree(new ByteArrayInputStream(byteContents), className)
  }
  def parse(b: ZaluumProtobuf.BoxClass, name: Option[Name] = None): BoxDef = {
    BoxDef(name.getOrElse(Name(b.getClassName)),
      pkg = if (b.hasPackage) Name(b.getPackage) else Name(""),
      superName = if (b.hasSuper) Some(Name(b.getSuper)) else None,
      guiSize = if (b.hasGuiSize) Some(parseDim(b.getGuiSize)) else None,
      image = if (b.hasImageName) Some(b.getImageName) else None,
      template = parse(b.getTemplate))
  }
  def parse(t: ZaluumProtobuf.Template): Template = {
    Template(
      blocks = t.getBlockList() map parse toList,
      ports = t.getPortList() map parse toList)
  }
  def parse(t: ZaluumProtobuf.Block): Block = {
    Block(junctions = t.getJunctionList map parse toList,
      valDefs = t.getInstanceList map parse toList,
      connections = t.getConnectionList map parse toList,
      parameters = List() /*FIXME*/ )
  }
  def parse(r: ZaluumProtobuf.Junction): Junction = {
    Junction(Name(r.getName), parse(r.getPos))
  }
  def parse(l: ZaluumProtobuf.Label): LabelDesc =
    LabelDesc(l.getDescription, parse(l.getPos).toVector)

  def parse(i: ZaluumProtobuf.Instance): ValDef = {
    val size = if (i.hasSize) Some(parseDim(i.getSize)) else None
    val lbl = if (i.hasLabel) Some(parse(i.getLabel)) else None
    val lblgui = if (i.hasLabelGui) Some(parse(i.getLabelGui)) else None
    i.getType() match {
      case ZaluumProtobuf.Instance.Type.ClassInstance =>
        val ci = i.getExtension(ZaluumProtobuf.ClassInstance.instance)
        val guiPos = if (ci.hasGuiPos) Some(parse(ci.getGuiPos)) else None
        val guiSize = if (ci.hasGuiSize) Some(parseDim(ci.getGuiSize)) else None
        ValDef(
          Name(i.getName),
          Name(ci.getClassName),
          parse(i.getPos),
          size,
          guiPos,
          guiSize,
          i.getParameterList.map { parse(_) }.toList,
          ci.getConstructorParameterList.toList,
          ci.getConstructorTypesList.map { Name(_) }.toList,
          lbl,
          lblgui)

      case ZaluumProtobuf.Instance.Type.StatInstance =>
        val si = i.getExtension(ZaluumProtobuf.StatInstance.instance)
        ???
    }
  }
  def parse(p: ZaluumProtobuf.Parameter): Param = {
    Param(Name(p.getKey), p.getValue)
  }
  def parse(c: ZaluumProtobuf.Connection): ConnectionDef = {
    ConnectionDef(
      parse(c.getSource),
      parse(c.getTarget),
      c.getPointList map { parse(_) } toList)
  }
  def parse(ref: ZaluumProtobuf.Ref): Option[ConnectionEnd] = {
    if (ref.hasPort) Some(parse(ref.getPort))
    else if (ref.hasJunction) Some(parse(ref.getJunction))
    else None
  }
  def parse(jref: ZaluumProtobuf.JunctionRef): JunctionRef = {
    JunctionRef(Name(jref.getName))
  }
  def parse(p: ZaluumProtobuf.PortRef): PortRef = {
    PortRef(if (p.hasBoxName) ValRef(Name(p.getBoxName)) else ThisRef(),
      Name(p.getPortName), if (p.hasIn) p.getIn else true)
  }
  def parse(p: ZaluumProtobuf.Port): PortDef = {
    PortDef(name = Name(p.getName), typeName = Name(p.getType),
      dir = parseDir(p.getDirection),
      inPos = parse(p.getPosInternal),
      extPos = parse(p.getPosExternal))
  }
  def parseDir(dir: ZaluumProtobuf.Direction) = dir match {
    case ZaluumProtobuf.Direction.IN ⇒ In
    case ZaluumProtobuf.Direction.OUT ⇒ Out
    case ZaluumProtobuf.Direction.SHIFT ⇒ Shift
  }
  def parse(p: ZaluumProtobuf.Point) = Point(p.getX, p.getY)
  def parseDim(p: ZaluumProtobuf.Point) = Dimension(p.getX, p.getY)
  /*def parse(p: ZaluumProtobuf.Clipboard): Clipboard = {
    val boxes = p.getBoxClassList map { parse(_) } toList
    val vals = p.getInstanceList map { parse(_) } toList
    val ports = p.getPortList map { parse(_) } toList
    val connections = p.getConnnectionList map { parse(_) } toList;
    Clipboard(boxes, vals, ports, connections)
  }*/
}
