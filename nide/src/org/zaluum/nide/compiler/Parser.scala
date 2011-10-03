package org.zaluum.nide.compiler

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.nio.charset.Charset

import scala.collection.JavaConversions.asScalaBuffer

import org.zaluum.nide.protobuf.ZaluumProtobuf
import org.zaluum.nide.zge.Clipboard

import com.google.protobuf.ExtensionRegistry

object Parser {
  def readTree(i: InputStream, className: Name): BoxDef = {
    val a = try {
      val proto = ZaluumProtobuf.BoxClass.parseFrom(i)
      parse(proto, Some(className))
    } catch {
      case e ⇒
        // TODO better handling
        println("PARSING ERROR! First after file creation is expected" + e.toString)
        BoxDef.emptyBox(className.classNameWithoutPackage.str, className.packageProxy)
    }
    a.assignLine(1)
    a
  }

  def readTree(isoString: String, className: Name): BoxDef = {
    val byteContents = isoString.getBytes(Charset.forName("ISO-8859-1")) // TODO ??
    readTree(new ByteArrayInputStream(byteContents), className)
  }
  def parse(b: ZaluumProtobuf.BoxClass, name: Option[Name] = None): BoxDef = {
    BoxDef(name.getOrElse(Name(b.getClassName)),
      pkg = if (b.hasPackage) Name(b.getPackage) else Name(""),
      guiSize = if (b.hasGuiSize) Some(parseDim(b.getGuiSize)) else None,
      image = if (b.hasImageName) Some(b.getImageName) else None,
      initMethod = if (b.hasInitMethod()) Some(b.getInitMethod()) else None,
      constructor = b.getConstructorParamList() map { parse } toList,
      template = parse(b.getTemplate))
  }
  def parse(p: ZaluumProtobuf.ParamDecl): VarDecl = {
    VarDecl(Name(p.getName), Name(if (p.hasType) p.getType() else ""))
  }
  def parse(t: ZaluumProtobuf.Template): Template = {
    Template(
      blocks = t.getBlockList() map parse toList,
      ports = t.getPortList() map parse toList,
      currentBlock = if (t.hasCurrentBlock()) Some(t.getCurrentBlock) else None)
  }
  def parse(t: ZaluumProtobuf.Block): Block = {
    Block(junctions = t.getJunctionList map parse toList,
      valDefs = t.getInstanceList map parse toList,
      connections = t.getConnectionList map parse toList,
      parameters = List() /*FIXME unused yet*/ )
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
    val template = if (i.hasTemplate) Some(parse(i.getTemplate())) else None
    ValDef(
      Name(i.getName),
      Name(i.getType),
      parse(i.getPos),
      size,
      i.getParameterList.map { parse(_) }.toList,
      lbl,
      lblgui,
      template)

  }
  def parse(p: ZaluumProtobuf.Parameter): Param = {
    import scala.collection.JavaConverters._
    Param(Name(p.getKey), p.getValueList().asScala.toList)
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
    case ZaluumProtobuf.Direction.IN    ⇒ In
    case ZaluumProtobuf.Direction.OUT   ⇒ Out
    case ZaluumProtobuf.Direction.SHIFT ⇒ Shift
  }
  def parse(p: ZaluumProtobuf.Point) = Point(p.getX, p.getY)
  def parseDim(p: ZaluumProtobuf.Point) = Dimension(p.getX, p.getY)
  def parse(p: ZaluumProtobuf.Clipboard): Clipboard = {
    val vals = p.getInstanceList map { parse(_) } toList
    val ports = p.getPortList map { parse(_) } toList
    val connections = p.getConnnectionList map { parse(_) } toList;
    Clipboard(vals, ports, connections)
  }
}
