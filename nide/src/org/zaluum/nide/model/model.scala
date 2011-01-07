package org.zaluum.nide.model

import com.google.common.base.Charsets
import java.io.OutputStreamWriter
import java.io.OutputStream
import java.io.InputStream
import org.zaluum.nide.protobuf.BoxFileProtos
import com.google.protobuf.TextFormat
import scala.collection.JavaConversions._
object Model {
  def toPoint(i:(Int,Int)) = {
    val p = BoxFileProtos.Contents.Point.newBuilder
    val (x,y) = i
    p.setX(x)
    p.setY(y)
    p.build
  }
  def fromPoint(p:BoxFileProtos.Contents.Point) =  (p.getX,p.getY)
}
object ProtoModel{
  def definitionToProtos(m:Model) = {
    val b =BoxFileProtos.Definition.newBuilder()
    b.setClassName(m.className)
    b.setImageName(m.imageName)
    b.build
  }
  def contentsToProtos(m:Model) = {
    val c = BoxFileProtos.Contents.newBuilder()
    for (b <- m.boxes) { c.addInstance(b.toInstance)  }
    // TODO connections
    c.build
  }
  def writeTo(m:Model,out: OutputStream) {
    definitionToProtos(m).writeDelimitedTo(out)
    contentsToProtos(m).writeDelimitedTo(out)
  }
  def writeTextTo(m:Model,out:OutputStream) {
    val o = new OutputStreamWriter(out,Charsets.UTF_8)
    TextFormat.print(definitionToProtos(m), o);
    TextFormat.print(contentsToProtos(m), o)
    o.flush
  }
  def readDefinition(in:InputStream) = { 
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    new BoxClass(definition.getClassName)
  }
  def read(in : InputStream) ={
    val model = new Model()
    val definition = BoxFileProtos.Definition.parseDelimitedFrom(in)
    model.className = definition.getClassName()
    model.imageName = definition.getImageName()
    val contents = BoxFileProtos.Contents.parseDelimitedFrom(in)
    for (instance <- contents.getInstanceList) {
      val box = new Box
      if (model.boxNamed(instance.getName)) 
        throw new Exception("Box name repeated" + instance.getName)
      box.name = instance.getName
      box.pos = (instance.getPos.getX, instance.getPos.getY)
      box.className = instance.getClassName
      model.boxes+=box
      // ports
    }
    for (connection <- contents.getConnectionList){
      // TODO disconnected ports
      connection.getSource
      connection.getTarget
      // lookup
    }
    model
  }
}

class Model {
  var className = ""
  var imageName = ""
  var boxes = Set[Box]()
  def boxNamed(str:String) = boxes.exists{ _.name == str  }
  var connections =Set[Connection]()
}

class Connection(var from: Option[Port], var to:Option[Port]) 
class Box {
  var className = "img"
  var pos = (0, 0)
  var name = ""
  var size = (30, 30)
  var ports = Set[Port]()
  def toInstance = {
    val instance = BoxFileProtos.Contents.Instance.newBuilder()
    instance.setName(name)
    instance.setClassName(className)
    instance.setPos(Model.toPoint(pos))
    instance.build
    // TODO ports
  }
}
class Port(var box:Box,var name:String) {
  var pos = (0,10)
}
class BoxClass(val className: String) {
  var ports = Set[TypedPort]()
}
case class TypedPort(val descriptor: String, val in: Boolean, val name: String)

abstract class Command {
  def act() = redo()
  def redo()
  def undo()
}
class ChainCommand(val commands: List[Command]) extends Command{
  override def act() { commands.foreach { _.act() } }
  def redo() { commands.foreach { _.redo() } }
  def undo() { commands.reverse.foreach { _.undo() } }
}
class MoveCommand(box: Box, pos: (Int, Int)) extends Command{
  var old = box.pos
  def redo() { box.pos = pos }
  def undo() { box.pos = old }
}
class ResizeCommand(box: Box, pos:(Int,Int),size: (Int,Int)) extends Command{
  val oldSize = box.size
  val oldPos = box.pos
  def redo() { box.size = size; box.pos = pos }
  def undo() { box.size = oldSize; box.pos = oldPos }
}
class CreateCommand(box:Box, model:Model) extends Command{
  def redo { model.boxes += box }
  def undo { model.boxes -= box }
}