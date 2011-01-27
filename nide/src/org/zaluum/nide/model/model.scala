package org.zaluum.nide.model

import javax.swing.JComponent
import org.zaluum.nide.protobuf.BoxFileProtos.BoxClassDef.Direction
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.annotation.tailrec
class BoxClassDecl(var className: String , var imageName: Option[String], var visual: Boolean, var guiSize:Dimension) extends Namer with BoxClassDeclLocator {
  var boxes = Set[Box]()
  var portDecls = Set[PortDecl]()
  var classDecls = Set[BoxClassDecl]()  
  var connections = Set[Connection]()
  
  def usedNames = boxes.map { _.name } ++ portDecls.map { _.name }
  def boxNamed(str: String) = boxes.exists { _.name == str }
  def findBox(str: String) = boxes find { _.name == str }
  def findPortDecl(str: String) = portDecls find { _.name == str }
  def cleanUp {
    connections = connections.filterNot { c ⇒
      c.from == c.to || // loop
      (c.buf.isEmpty && !(c.from.isDefined && c.from.isDefined))
    } //empty waypoints
  }
  def toProtoDefinition = {
    val b = BoxFileProtos.BoxClassDef.newBuilder()
    b.setImageName(imageName.getOrElse(""))
    def sortInOut(in: Boolean) {
      val sorted = (portDecls filter { p: PortDecl ⇒ p.in == in }).toList.sortBy(_.pos.y)
      val x = if (in) 0 else 48
      sorted.view.zipWithIndex foreach { case (p, i) ⇒ p.posExternal = Point(x, i * 8) }
    }
    b.setGuiSize(guiSize.toProto)
    b.setVisual(visual)
    sortInOut(in = true)
    sortInOut(in = false)
    for (portDecl ← portDecls) { b.addPort(portDecl.toProto) }
    b.build
  }
  def toProtoContents = {
    val c = BoxFileProtos.BoxClassDef.newBuilder()
    // TODO internal class decls
    for (b ← boxes) { c.addInstance(b.toProto) }
    for (con ← connections) { c.addConnection(con.toProto) }
    c.build
  }
}

class Box(
    //var boxClassDecl:BoxClassDecl,
    var boxClassRef:BoxClassRef,
    var name :String, 
    var pos:Point,
    var guiPos: Option[Resizable] = None)  extends Positionable with Locatable {
  def location = Location(name)
  override def toString = name
  def toProto = {
    val instance = BoxFileProtos.BoxClassDef.Instance.newBuilder()
    instance.setName(name)
    instance.setClassName(boxClassRef.toString)
    instance.setPos(pos.toProto)
    guiPos foreach { gp ⇒
      instance.setGuiPos(gp.pos.toProto)
      instance.setGuiSize(gp.size.toProto)
    }
    instance.build
  }
}

class PortDecl( 
    var bcd: BoxClassDecl,
    var name: String, 
    var in: Boolean, 
    var descriptor: String,
    var pos :Point = Point(0, 10),
    var posExternal : Point = Point(0,0)) extends Positionable with Locatable {
  def location = Location("#" + name)
  override def toString = "portDecl(" + name + ")"
  def toProto = {
    val port = BoxFileProtos.BoxClassDef.Port.newBuilder()
    port.setDirection(if (in) Direction.IN else Direction.OUT)
    port.setPosInternal(pos.toProto)
    port.setPosExternal(posExternal.toProto)
    port.setName(name)
    port.setType(descriptor)
  }
}
sealed trait BoxClassRef 
case class ExtBoxClassRef(className:String) extends BoxClassRef{
  override def toString = className
}
case class IntBoxClassRef(boxClassDecl:BoxClassRef, className:String) extends BoxClassRef{
  override def toString = boxClassDecl + "$" + className 
}