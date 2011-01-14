package org.zaluum.nide.model

import org.zaluum.nide.protobuf.BoxFileProtos.Definition.Direction
import com.google.common.base.Charsets
import java.io.InputStream
import com.google.protobuf.TextFormat
import java.io.OutputStreamWriter
import java.io.OutputStream
import org.zaluum.nide.protobuf.BoxFileProtos
import scala.collection.JavaConversions._

object ProtoModel {
  def definitionToProtos(m: Model) = {
    val b = BoxFileProtos.Definition.newBuilder()
    b.setClassName(m.className)
    b.setImageName(m.imageName)
    for (portDecl ← m.portDecls) { b.addPort(portDecl.toProto) }

    b.build
  }
  def contentsToProtos(m: Model) = {
    val c = BoxFileProtos.Contents.newBuilder()
    for (b ← m.boxes) { c.addInstance(b.toProto) }
    for (con ← m.connections) { c.addConnection(con.toProto) }
    // TODO connections
    c.build
  }
  def writeTo(m: Model, out: OutputStream) {
    definitionToProtos(m).writeDelimitedTo(out)
    contentsToProtos(m).writeDelimitedTo(out)
  }
  def writeTextTo(m: Model, out: OutputStream) {
    val o = new OutputStreamWriter(out, Charsets.UTF_8)
    TextFormat.print(definitionToProtos(m), o);
    TextFormat.print(contentsToProtos(m), o)
    o.flush
  }
  def readDefinition(in: InputStream) = {
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    new BoxClass(definition.getClassName, false, definition.getImageName)
  }
  def read(in: InputStream) = {
    val model = new Model()
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    model.className = definition.getClassName()
    model.imageName = definition.getImageName()
    for (port ← definition.getPortList) {
      PortDecl(model, port.getName, port.getDirection == Direction.IN) // TODO type
    }

    val contents = BoxFileProtos.Contents.parseDelimitedFrom(in)
    for (instance ← contents.getInstanceList) {
      val box = new Box
      if (model.boxNamed(instance.getName))
        throw new Exception("Box name repeated" + instance.getName)
      Box(model, name = instance.getName, pos = Point(instance.getPos.getX, instance.getPos.getY),
        className = instance.getClassName)
      // ports
    }
    var portRefs = Map[Box, Set[PortRef]]()
    for (connection ← contents.getConnectionList) {
      if (connection.hasSource && connection.hasTarget) {
        def findBox(str: String) = model.boxes find { _.name == str }
        def findBoxPortRef(box: Box, name: String) = {
          if (!portRefs.contains(box)) portRefs += (box -> Set())
          portRefs(box).find { _.name == name } getOrElse {
            val newRef = BoxPortRef(box, name) // FIXME modelPortRef
            portRefs += (box -> (portRefs(box) + newRef))
            newRef
          }
        }
        def findModelPortRef(str:String) = model.findPortDecl(str) map { _=>ModelPortRef(str) }
        def portOf(proto: BoxFileProtos.Contents.PortRef) = {
          if (proto.hasBoxName)
            findBox(proto.getBoxName) map { b ⇒ findBoxPortRef(b, proto.getPortName) }
          else 
            findModelPortRef(proto.getPortName)
        }
        (portOf(connection.getSource), portOf(connection.getTarget)) match {
          case (Some(from), Some(to)) ⇒
            model.connections += new Connection(Some(from), Some(to))
          case (f, t) ⇒ println("ports not found " + f + " " + t)
        }
      }
    }
    model.cleanUp
    model
  }
}