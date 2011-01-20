package org.zaluum.nide.model

import java.io.ByteArrayOutputStream
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
    b.setImageName(m.imageName)
    def sortInOut(in:Boolean) {
      val sorted = (m.portDecls filter { p:PortDecl=>p.in==in }).toList.sortBy (_.pos.y)
      val x = if (in) 0 else 48
      sorted.view.zipWithIndex foreach { case (p,i) => p.posExternal = Point(x,i*8)}
    }
    sortInOut(in=true)
    sortInOut(in=false)
    for (portDecl ← m.portDecls) { b.addPort(portDecl.toProto) }
    b.build
  }
  def contentsToProtos(m: Model) = {
    val c = BoxFileProtos.Contents.newBuilder()
    for (b ← m.boxes) { c.addInstance(b.toProto) }
    for (con ← m.connections) { c.addConnection(con.toProto) }
    c.build
  }
  def writeTo(m: Model, out: OutputStream) {
    definitionToProtos(m).writeDelimitedTo(out)
    contentsToProtos(m).writeDelimitedTo(out)
  }
  def toByteArray(m:Model) = {
    val out = new ByteArrayOutputStream()
    writeTo(m,out)
    out.toByteArray
  }
  def writeTextTo(m: Model, out: OutputStream) {
    val o = new OutputStreamWriter(out, Charsets.UTF_8)
    TextFormat.print(definitionToProtos(m), o);
    TextFormat.print(contentsToProtos(m), o)
    o.flush
  }
  def readDefinition(in: InputStream, className:String) = {
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    val boxClass = new BoxClass(className, false, definition.getImageName)
    for ( port <- definition.getPortList) {
      boxClass.ports += TypedPort(port.getType, port.getDirection==Direction.IN, port.getName, fromPoint(port.getPosExternal))
    }
    boxClass
  }
  def read(in: InputStream, className:String) = {
    val model = new Model()
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    model.className = className
    model.imageName = if (definition.hasImageName) definition.getImageName() else ""
    for (port ← definition.getPortList) {
      PortDecl(model, port.getName, 
          port.getDirection == Direction.IN, 
          fromPoint(port.getPosInternal),
          fromPoint(port.getPosExternal)) // TODO type
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
  def toPoint(i:Point) = {
    val p = BoxFileProtos.Point.newBuilder
    p.setX(i.x)
    p.setY(i.y)
    p.build
  }
  def fromPoint(p:BoxFileProtos.Point) =  Point(p.getX,p.getY)

}