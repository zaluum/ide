package org.zaluum.nide.compiler

import java.nio.charset.Charset

import scala.collection.JavaConversions.seqAsJavaList

import org.zaluum.nide.protobuf.ZaluumProtobuf
import org.zaluum.nide.zge.Clipboard
object Serializer {
  val version = "ZALUUM-1.0"
  def writeToIsoString(d: ZaluumProtobuf.BoxClass): String =
    new String(d.toByteArray, Charset.forName("ISO-8859-1"))

  def proto(b: BoxDef): ZaluumProtobuf.BoxClass = {
    val p = ZaluumProtobuf.BoxClass.newBuilder
    p.setVersion(version)
    p.setClassName(b.name.str)
    p.setPackage(b.pkg.str)
    b.image foreach { p.setImageName(_) }
    p.setTemplate(proto(b.template))
    p.setGuiSize(proto(b.guiSize.getOrElse(Dimension(100, 100))))
    p.setVisual(false) // TODO
    b.initMethod foreach { p.setInitMethod }
    b.constructor foreach { t ⇒ p.addConstructorParam(proto(t)) }
    p.build
  }
  def proto(v: VarDecl): ZaluumProtobuf.ParamDecl = {
    val p = ZaluumProtobuf.ParamDecl.newBuilder
    p.setName(v.name.str)
    p.setType(v.tpeName.str)
    p.build
  }
  def proto(t: Template): ZaluumProtobuf.Template = {
    val p = ZaluumProtobuf.Template.newBuilder
    t.ports foreach { port ⇒ p.addPort(proto(port)) }
    t.blocks foreach { b ⇒ p.addBlock(proto(b)) }
    t.currentBlock foreach { c ⇒ p.setCurrentBlock(c) }
    p.build
  }
  def proto(b: Block): ZaluumProtobuf.Block = {
    val p = ZaluumProtobuf.Block.newBuilder()
    b.valDefs foreach { va ⇒ p.addInstance(proto(va)) }
    b.connections foreach { c ⇒ p.addConnection(proto(c)) } // TODO sort
    b.junctions foreach { j ⇒ p.addJunction(proto(j)) }
    //b.parameters // FIXME unused
    p.build()
  }
  def proto(l: LabelDesc): ZaluumProtobuf.Label = {
    ZaluumProtobuf.Label.newBuilder
      .setDescription(l.description)
      .setPos(proto(l.pos.toPoint))
      .build
  }
  def proto(p: ValDef): ZaluumProtobuf.Instance = {
    import scala.collection.JavaConversions._
    val b = ZaluumProtobuf.Instance.newBuilder
    p.params collect { case p: Param ⇒ p } foreach { p ⇒ b.addParameter(proto(p)) }
    p.label foreach { l ⇒ b.setLabel(proto(l)) }
    p.labelGui foreach { l ⇒ b.setLabelGui(proto(l)) }
    p.template foreach { t ⇒ b.setTemplate(proto(t)) }
    b.setType(p.typeName.str)
      .setName(p.name.str)
      .setPos(proto(p.pos))
      .setSize(proto(p.size.getOrElse(Dimension(50, 50))))
      .build
  }
  def proto(j: Junction): ZaluumProtobuf.Junction = {
    ZaluumProtobuf.Junction.newBuilder.setName(j.name.str).setPos(proto(j.p)).build
  }
  def proto(p: Param): ZaluumProtobuf.Parameter = {
    ZaluumProtobuf.Parameter.newBuilder
      .setKey(p.key.str)
      .addAllValue(p.values).build
  }
  def proto(c: ConnectionDef): ZaluumProtobuf.Connection = {
    val b = ZaluumProtobuf.Connection.newBuilder
    b.setSource(protoRef(c.a))
    b.setTarget(protoRef(c.b))
    c.points foreach { p ⇒ b.addPoint(proto(p)) }
    b.build
  }
  def protoRef(t: Option[ConnectionEnd]): ZaluumProtobuf.Ref = {
    t match {
      case Some(JunctionRef(name)) ⇒
        ZaluumProtobuf.Ref.newBuilder.setJunction(
          ZaluumProtobuf.JunctionRef.newBuilder
            .setName(name.str)
            .build).build
      case Some(PortRef(ThisRef(), name, in)) ⇒
        ZaluumProtobuf.Ref.newBuilder.setPort(
          ZaluumProtobuf.PortRef.newBuilder
            .setPortName(name.str)
            .setIn(in)
            .build).build
      case Some(PortRef(ValRef(boxName), name, in)) ⇒
        ZaluumProtobuf.Ref.newBuilder.setPort(
          ZaluumProtobuf.PortRef.newBuilder
            .setBoxName(boxName.str)
            .setPortName(name.str)
            .setIn(in)
            .build).build
      case None ⇒ ZaluumProtobuf.Ref.newBuilder.build
    }
  }
  def proto(p: PortDef): ZaluumProtobuf.Port = {
    ZaluumProtobuf.Port.newBuilder
      .setDirection(proto(p.dir))
      .setName(p.name.str)
      .setPosExternal(proto(p.extPos))
      .setPosInternal(proto(p.inPos))
      .setType(p.typeName.str)
      .build
  }
  def proto(p: Point): ZaluumProtobuf.Point =
    ZaluumProtobuf.Point.newBuilder.setX(p.x).setY(p.y).build;
  def proto(d: Dimension): ZaluumProtobuf.Point =
    ZaluumProtobuf.Point.newBuilder.setX(d.w).setY(d.h).build;

  def proto(d: PortDir): ZaluumProtobuf.Direction = d match {
    case In    ⇒ ZaluumProtobuf.Direction.IN
    case Out   ⇒ ZaluumProtobuf.Direction.OUT
    case Shift ⇒ ZaluumProtobuf.Direction.SHIFT
  }
  import scala.collection.JavaConversions._
  def proto(c: Clipboard): ZaluumProtobuf.Clipboard = {
    ZaluumProtobuf.Clipboard.newBuilder
      .addAllInstance(c.valDefs map { proto(_) })
      .addAllPort(c.ports map { proto(_) })
      .addAllConnnection(c.connections map { proto(_) })
      .build;
  }
}