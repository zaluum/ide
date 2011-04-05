package org.zaluum.nide.compiler

import org.zaluum.nide.zge.H
import org.zaluum.nide.protobuf.BoxFileProtos
object Serializer {
  def proto(b: BoxDef): BoxFileProtos.BoxClassDef = {
    val p = BoxFileProtos.BoxClassDef.newBuilder
    p.setClassName(b.name.str)
    b.superName foreach { n ⇒ p.setSuper(n.str) }
    b.image foreach { p.setImageName(_) }
    b.defs collect { case d: BoxDef ⇒ d } sortBy { _.name.str } foreach { d ⇒ p.addInnerClass(proto(d)) }
    b.ports collect { case port: PortDef ⇒ port } sortBy { _.name.str } foreach { port ⇒ p.addPort(proto(port)) }
    b.vals collect { case va: ValDef ⇒ va } sortBy { _.name.str } foreach { va ⇒ p.addInstance(proto(va)) }
    b.connections collect { case c: ConnectionDef ⇒ p.addConnection(proto(c)) } // TODO sort
    b.junctions collect { case j: Junction ⇒ p.addJunction(proto(j)) }
    p.setGuiSize(proto(b.guiSize.getOrElse(Dimension(100,100))))  
    p.setVisual(false) // TODO
    p.build
  }

  def proto(p: ValDef): BoxFileProtos.BoxClassDef.Instance = {
    val b = BoxFileProtos.BoxClassDef.Instance.newBuilder
    p.params collect { case p: Param ⇒ p } sortBy { _.key.str } foreach { p ⇒ b.addParameter(proto(p)) }
    b.setGuiPos(proto(p.guiPos.getOrElse { Point(0, 0) }))
      .setGuiSize(proto(p.guiSize.getOrElse { Dimension(50, 50) }))
      .setClassName(p.typeName.str)
      .setName(p.name.str)
      .setPos(proto(p.pos))
      .setSize(proto(p.size.getOrElse(Dimension(50, 50))))
      .build
  }
  def proto(j: Junction): BoxFileProtos.BoxClassDef.Junction = {
    BoxFileProtos.BoxClassDef.Junction.newBuilder.setName(j.name.str).setPos(proto(j.p)).build
  }
  def proto(p: Param): BoxFileProtos.BoxClassDef.Parameter = {
    BoxFileProtos.BoxClassDef.Parameter.newBuilder
      .setKey(p.key.str)
      .setValue(p.value).build
  }
  def proto(c: ConnectionDef): BoxFileProtos.BoxClassDef.Connection = {
    val b = BoxFileProtos.BoxClassDef.Connection.newBuilder
    b.setSource(protoRef(c.a)) 
    b.setTarget(protoRef(c.b)) 
    c.points foreach { p ⇒ b.addPoint(proto(p)) }
    b.build
  }
  def protoRef(t: Tree): BoxFileProtos.BoxClassDef.Ref = {
    t match {
      case JunctionRef(name) ⇒
        BoxFileProtos.BoxClassDef.Ref.newBuilder.setJunction(
          BoxFileProtos.BoxClassDef.JunctionRef.newBuilder
          .setName(name.str)
          .build).build
      case PortRef(ThisRef(), name, in) ⇒
        BoxFileProtos.BoxClassDef.Ref.newBuilder.setPort(
          BoxFileProtos.BoxClassDef.PortRef.newBuilder
          .setPortName(name.str)
          .setIn(in)
          .build).build
      case PortRef(ValRef(boxName), name, in) ⇒
        BoxFileProtos.BoxClassDef.Ref.newBuilder.setPort(
          BoxFileProtos.BoxClassDef.PortRef.newBuilder
          .setBoxName(boxName.str)
          .setPortName(name.str)
          .setIn(in)
          .build).build
      case EmptyTree ⇒ BoxFileProtos.BoxClassDef.Ref.newBuilder.build
    }
  }
  def proto(p: PortDef): BoxFileProtos.BoxClassDef.Port = {
    BoxFileProtos.BoxClassDef.Port.newBuilder
      .setDirection(proto(p.dir))
      .setName(p.name.str)
      .setPosExternal(proto(p.extPos))
      .setPosInternal(proto(p.inPos))
      .setType(p.typeName.str)
      .build
  }
  def proto(p: Point): BoxFileProtos.BoxClassDef.Point =
    BoxFileProtos.BoxClassDef.Point.newBuilder.setX(p.x).setY(p.y).build;
  def proto(d: Dimension): BoxFileProtos.BoxClassDef.Point =
    BoxFileProtos.BoxClassDef.Point.newBuilder.setX(d.w).setY(d.h).build;

  def proto(d: PortDir): BoxFileProtos.BoxClassDef.Direction = d match {
    case In ⇒ BoxFileProtos.BoxClassDef.Direction.IN
    case Out ⇒ BoxFileProtos.BoxClassDef.Direction.OUT
    case Shift ⇒ BoxFileProtos.BoxClassDef.Direction.SHIFT
  }
}