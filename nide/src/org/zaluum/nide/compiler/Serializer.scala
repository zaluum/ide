package org.zaluum.nide.compiler

import org.zaluum.nide.protobuf.BoxFileProtos
object Serializer {
  def proto(b:BoxDef) : BoxFileProtos.BoxClassDef = {
      val p = BoxFileProtos.BoxClassDef.newBuilder
      p.setClassName(b.name.str)
      b.image foreach { p.setImageName(_) }
      b.defs collect { case d:BoxDef => d} sortBy {_.name.str} foreach { d=>p.addInnerClass(proto(d)) }
      b.ports collect  { case port:PortDef => port } sortBy {_.name.str} foreach {port => p.addPort(proto(port)) }
      b.vals collect { case va:ValDef => va } sortBy {_.name.str} foreach { va => p.addInstance(proto(va)) }
      b.connections collect { case c:ConnectionDef => p.addConnection(proto(c)) } // TODO sort
      p.setGuiSize(proto(Dimension(400,400))) // TODO
      p.setVisual(false) // TODO
      p.build
  }
  
  def proto (p:ValDef) : BoxFileProtos.BoxClassDef.Instance = {
      BoxFileProtos.BoxClassDef.Instance.newBuilder
      .setGuiPos(proto(p.guiPos.getOrElse {Point(0,0)}))
      .setGuiSize(proto(p.guiSize.getOrElse {Dimension(50,50)}))
      .setClassName(p.typeName.str )
      .setName(p.name.str)
      .setPos(proto(p.pos))
      .setSize(proto(p.size.getOrElse(Dimension(50,50))))
      .build
  }
  def proto (c:ConnectionDef) : BoxFileProtos.BoxClassDef.Connection = {
   val b = BoxFileProtos.BoxClassDef.Connection.newBuilder
   protoRef(c.a) foreach { b.setSource(_) }
   protoRef(c.b) foreach { b.setTarget(_) }
   b.build
  }
  /*def parse(p: BoxFileProtos.BoxClassDef.PortRef): PortRef = {
    PortRef(if (p.hasBoxName) ValRef(Name(p.getBoxName)) else ThisRef,
      Name(p.getPortName),if (p.hasIn) p.getIn else true)
  }*/
  def protoRef (t:Tree) : Option[BoxFileProtos.BoxClassDef.PortRef] = {
    t match {
      case PortRef(ThisRef, name, in) => 
        Some(BoxFileProtos.BoxClassDef.PortRef.newBuilder
        .setPortName(name.str)
        .setIn(in)
        .build)
      case PortRef(ValRef(boxName), name, in) =>
        Some(BoxFileProtos.BoxClassDef.PortRef.newBuilder
        .setBoxName(boxName.str)
        .setPortName(name.str)
        .setIn(in)
        .build)
      case EmptyTree => None
      case _ => throw new Exception("unexpected box ref " +t )
    }
  }
  def proto (p:PortDef) : BoxFileProtos.BoxClassDef.Port = { 
   BoxFileProtos.BoxClassDef.Port.newBuilder
     .setDirection(proto(p.dir))
     .setName(p.name.str)
     .setPosExternal(proto(p.extPos))
     .setPosInternal(proto(p.inPos))
     .setType(p.typeName.str)
     .build
  }
  def proto (p: Point) : BoxFileProtos.BoxClassDef.Point = 
    BoxFileProtos.BoxClassDef.Point.newBuilder.setX(p.x).setY(p.y).build;
  def proto (d: Dimension) : BoxFileProtos.BoxClassDef.Point = 
    BoxFileProtos.BoxClassDef.Point.newBuilder.setX(d.w).setY(d.h).build;

  def proto (d:PortDir) : BoxFileProtos.BoxClassDef.Direction = d match {
    case In => BoxFileProtos.BoxClassDef.Direction.IN
    case Out => BoxFileProtos.BoxClassDef.Direction.OUT
    case Shift => BoxFileProtos.BoxClassDef.Direction.SHIFT
  }
}