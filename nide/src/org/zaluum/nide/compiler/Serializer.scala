package org.zaluum.nide.compiler

import org.zaluum.nide.zge.Clipboard
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

  def proto(p: ValDef): BoxFileProtos.Instance = {
    import scala.collection.JavaConversions._
    val b = BoxFileProtos.Instance.newBuilder
    p.params collect { case p: Param ⇒ p } sortBy { _.key.str } foreach { p ⇒ b.addParameter(proto(p)) }
    b.setGuiPos(proto(p.guiPos.getOrElse { Point(0, 0) }))
      .setGuiSize(proto(p.guiSize.getOrElse { Dimension(50, 50) }))
      .setClassName(p.typeName.str)
      .setName(p.name.str)
      .setPos(proto(p.pos))
      .setSize(proto(p.size.getOrElse(Dimension(50, 50))))
      .addAllConstructorParameter(p.constructorParams)
      .addAllConstructorTypes(p.constructorTypes map {_.str })
      .build
  }
  def proto(j: Junction): BoxFileProtos.Junction = {
    BoxFileProtos.Junction.newBuilder.setName(j.name.str).setPos(proto(j.p)).build
  }
  def proto(p: Param): BoxFileProtos.Parameter = {
    BoxFileProtos.Parameter.newBuilder
      .setKey(p.key.str)
      .setValue(p.value).build
  }
  def proto(c: ConnectionDef): BoxFileProtos.Connection = {
    val b = BoxFileProtos.Connection.newBuilder
    b.setSource(protoRef(c.a)) 
    b.setTarget(protoRef(c.b)) 
    c.points foreach { p ⇒ b.addPoint(proto(p)) }
    b.build
  }
  def protoRef(t: Tree): BoxFileProtos.Ref = {
    t match {
      case JunctionRef(name) ⇒
        BoxFileProtos.Ref.newBuilder.setJunction(
          BoxFileProtos.JunctionRef.newBuilder
          .setName(name.str)
          .build).build
      case PortRef(ThisRef(), name, in) ⇒
        BoxFileProtos.Ref.newBuilder.setPort(
          BoxFileProtos.PortRef.newBuilder
          .setPortName(name.str)
          .setIn(in)
          .build).build
      case PortRef(ValRef(boxName), name, in) ⇒
        BoxFileProtos.Ref.newBuilder.setPort(
          BoxFileProtos.PortRef.newBuilder
          .setBoxName(boxName.str)
          .setPortName(name.str)
          .setIn(in)
          .build).build
      case EmptyTree ⇒ BoxFileProtos.Ref.newBuilder.build
    }
  }
  def proto(p: PortDef): BoxFileProtos.Port = {
    BoxFileProtos.Port.newBuilder
      .setDirection(proto(p.dir))
      .setName(p.name.str)
      .setPosExternal(proto(p.extPos))
      .setPosInternal(proto(p.inPos))
      .setType(p.typeName.str)
      .build
  }
  def proto(p: Point): BoxFileProtos.Point =
    BoxFileProtos.Point.newBuilder.setX(p.x).setY(p.y).build;
  def proto(d: Dimension): BoxFileProtos.Point =
    BoxFileProtos.Point.newBuilder.setX(d.w).setY(d.h).build;

  def proto(d: PortDir): BoxFileProtos.Direction = d match {
    case In ⇒ BoxFileProtos.Direction.IN
    case Out ⇒ BoxFileProtos.Direction.OUT
    case Shift ⇒ BoxFileProtos.Direction.SHIFT
  }
  import scala.collection.JavaConversions._
  def proto(c:Clipboard):BoxFileProtos.Clipboard = {
    BoxFileProtos.Clipboard.newBuilder
      .addAllInstance(c.valDefs map { proto(_)})
      .addAllBoxClass(c.boxes map { proto(_)})
      .addAllPort(c.ports map { proto(_)})
      .addAllConnnection(c.connections map { proto(_)})
      .build;
  }
}