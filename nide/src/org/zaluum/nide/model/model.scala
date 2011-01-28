package org.zaluum.nide.model

import javax.swing.JComponent
import org.zaluum.nide.protobuf.BoxFileProtos.BoxClassDef.Direction
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.annotation.tailrec
class BoxClassDecl(var className: BoxClassName, var imageName: Option[String], var visual: Boolean, var guiSize: Dimension) extends Namer with BoxClassDeclLocator {
  var boxes = Set[Box]()
  var portDecls = Set[PortDecl]()
  var innerClassDecls = Set[BoxClassDecl]()
  var connections = Set[Connection]()
  def innerClassesNames = innerClassDecls.map {_.className } collect { case InnerBoxClassName(_,name) => name }
  def usedNames = boxes.map { _.name } ++ portDecls.map { _.name } ++ innerClassesNames
  def boxNamed(str: String) = boxes.exists { _.name == str }
  def findBox(str: String) = boxes find { _.name == str }
  def findPortDecl(str: String) = portDecls find { _.name == str }
  def cleanUp {
    connections = connections.filterNot { c ⇒
      c.from == c.to || // loop
        (c.buf.isEmpty && !(c.from.isDefined && c.from.isDefined))
    } //empty waypoints
  }
  def toProto : BoxFileProtos.BoxClassDef = {
    val b = BoxFileProtos.BoxClassDef.newBuilder()
    className match {
      case InnerBoxClassName(_,name) => b.setClassName(name)
      case _ =>
    }
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
    for (inner <- innerClassDecls) { b.addInnerClass(inner.toProto)}
    for (box ← boxes) { b.addInstance(box.toProto) }
    for (con ← connections) { b.addConnection(con.toProto) }
    b.build
  }
}

class Box(
  //var boxClassDecl:BoxClassDecl,
  var boxClassName: BoxClassName,
  var name: String,
  var pos: Point,
  var guiPos: Option[Resizable] = None) extends Positionable with Locatable {
  def location = Location(name)
  override def toString = name
  def toProto = {
    val instance = BoxFileProtos.BoxClassDef.Instance.newBuilder()
    instance.setName(name)
    instance.setClassName(boxClassName.toString)
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
  var pos: Point = Point(0, 10),
  var posExternal: Point = Point(0, 0)) extends Positionable with Locatable {
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
object BoxClassName {
  def parse(str:String) : BoxClassName = {
    if (str.contains('$')) {
      def fullName(name:BoxClassName, names : List[String]):BoxClassName = {
        if (names.isEmpty) name 
        else fullName(InnerBoxClassName(name,names(0)),names.drop(1))
      }
      val names = str.split('$').toList
      fullName (ExtBoxClassName(names(0)), names.drop(1))
    }else{
      ExtBoxClassName(str) 
    }
  }
}
sealed trait BoxClassName {
  def isFullyQualifiedClassname: Boolean
  protected def checkCharStart(c: Char) = Character.isJavaIdentifierStart(c) || Character.isIdentifierIgnorable(c)
  protected def checkCharPart(c: Char) = Character.isJavaIdentifierPart(c) || Character.isIdentifierIgnorable(c)
  protected def partOk(part: String) = {
    !part.isEmpty &&
      checkCharStart(part(0)) &&
      part.view(1, part.length).forall { checkCharPart(_) }
  }
  def toRelativePathClass = toRelativePath + ".class"
  def toRelativePath = toString.replace(".", "/") 
  def classNameWithoutPackage = toString.split(".").lastOption
  def internal = toString.replace('.','/')
  def descriptor = "L" + internal + ";"
}
case class ExtBoxClassName(className: String) extends BoxClassName {
  override def toString = className
  private def partClassname = className.split("[\\.]").toList;
  def isFullyQualifiedClassname = {
    def checkParts = {
      val parts = partClassname
      parts.length != 0 && parts.forall {partOk(_)}
    }
    className != null && checkParts
  }
}
case class InnerBoxClassName(parent: BoxClassName, className: String) extends BoxClassName {
  if (className==null || className == "") throw new Exception
  override def toString = parent + "$" + className
  def isFullyQualifiedClassname = {
    parent.isFullyQualifiedClassname && partOk(className)
  }

}