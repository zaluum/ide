package org.zaluum.nide.model

import org.zaluum.nide.compiler.TypedPort
import org.zaluum.nide.compiler.BoxClass
import org.zaluum.nide.protobuf.BoxFileProtos.BoxClassDef.Direction
import com.google.common.base.Charsets
import com.google.protobuf.TextFormat
import java.io.{ OutputStream, OutputStreamWriter, InputStream, ByteArrayOutputStream }
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.JavaConversions._
import org.zaluum.nide.compiler.BoxClassPath

object ProtoBuffers {
  
  def toBoxClassDecl(definition: BoxFileProtos.BoxClassDef, className: BoxClassName): BoxClassDecl = {
    val bcd = new BoxClassDecl(className,
      imageName = if (definition.hasImageName) Some(definition.getImageName()) else None,
      guiSize = Dimension(definition.getGuiSize.getX, definition.getGuiSize.getY),
      visual = definition.getVisual)
    for (port ← definition.getPortList) {
      bcd.portDecls += new PortDecl(bcd,
        name = port.getName,
        in = port.getDirection == Direction.IN,
        descriptor = port.getType,
        pos = readPoint(port.getPosInternal),
        posExternal = readPoint(port.getPosExternal))
    }
    if (className.isInstanceOf[InnerBoxClassName] && !definition.hasClassName)
      throw new Exception("No name for inner class")
    for (pbcd ← definition.getInnerClassList) {
      bcd.innerClassDecls += toBoxClassDecl(pbcd, InnerBoxClassName(className, pbcd.getClassName))
    }
    for (instance ← definition.getInstanceList) {
      if (bcd.boxNamed(instance.getName)) throw new Exception("Box name repeated" + instance.getName)
      bcd.boxes += new Box(
        name = instance.getName,
        pos = Point(instance.getPos.getX, instance.getPos.getY),
        boxClassName = BoxClassName.parse(instance.getClassName),
        guiPos = if (instance.hasGuiPos && instance.hasGuiSize) {
          Some(new Resizable {
            var pos = Point(instance.getGuiPos.getX, instance.getGuiPos.getY)
            var size = Dimension(instance.getGuiSize.getX, instance.getGuiSize.getY)
          })
        } else None)
    }
    var portRefs = Map[Box, Set[PortRef]]()
    for (connection ← definition.getConnectionList) {
      if (connection.hasSource && connection.hasTarget) {
        def findBox(str: String) = bcd.boxes find { _.name == str }
        def findBoxPortRef(box: Box, name: String) = {
          if (!portRefs.contains(box)) portRefs += (box -> Set())
          portRefs(box).find { _.name == name } getOrElse {
            val newRef = BoxPortRef(box, name)
            portRefs += (box -> (portRefs(box) + newRef))
            newRef
          }
        }
        def findModelPortRef(str: String) = bcd.findPortDecl(str) map { _ ⇒ ModelPortRef(str) }
        def portOf(proto: BoxFileProtos.BoxClassDef.PortRef) = {
          if (proto.hasBoxName)
            findBox(proto.getBoxName) map { b ⇒ findBoxPortRef(b, proto.getPortName) }
          else
            findModelPortRef(proto.getPortName)
        }
        (portOf(connection.getSource), portOf(connection.getTarget)) match {
          case (Some(from), Some(to)) ⇒
            bcd.connections += new Connection(Some(from), Some(to))
          case (f, t) ⇒ println("ports not found " + f + " " + t)
        }
      }
    }
    bcd.cleanUp
    bcd

  }
  def readBoxClassDecl(in: InputStream, className: BoxClassName): BoxClassDecl = {
    val definition = BoxFileProtos.BoxClassDef.parseDelimitedFrom(in)
    toBoxClassDecl(definition, className)
  }
  def readPoint(p: BoxFileProtos.BoxClassDef.Point) = Point(p.getX, p.getY)
  def writeTo(bc: BoxClassDecl, out: OutputStream) {
    bc.toProto.writeDelimitedTo(out)
  }
  def toByteArray(bc: BoxClassDecl) = {
    val out = new ByteArrayOutputStream()
    writeTo(bc, out)
    out.toByteArray
  }
}