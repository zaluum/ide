package org.zaluum.runtime
import PersistentModel._
trait Command {
  def undo
  def redo
}
case class ResizeCommand(val r: Resizable, val pos: (Int,Int), val size:(Int,Int)) extends Command{
  val oldPos = r.pos
  val oldSize = r.size
  def redo = {r.pos = pos ; r.size= size}
  def undo = {r.pos = oldPos; r.size = oldSize}
}
case class PositionCommand(val p: Positional, val pos: (Int,Int)) extends Command{
  val oldPos = p.pos
  def redo = {p.pos = pos}
  def undo = {p.pos = oldPos}
}
case class CreateBoxCommand(val parent:ComposedPBox, val box:PBox) extends Command{
  def undo = {
    box.parent = null
    parent.boxes -= box
    parent.notifyObservers
  }
  def redo = {
    box.parent = parent
    parent.boxes += box
    box.uniqueName(box.name)
    parent.notifyObservers
  }
}
case class ComposedCommand(val commands : List[Command]) extends Command{
  def redo = commands foreach {_.redo }
  def undo = commands.reverse foreach { _.undo }
}
case class DeleteWireCommand(val w : PWire, val p : ComposedPBox) extends Command{
  def notifyObservers(){
    p.notifyObservers
    w.from .notifyObservers
    w.to .notifyObservers    
  }
  def redo {
    p.connections -= w
    notifyObservers()
  }
  def undo {
    p.connections += w
    notifyObservers()
  }
}
case class DeleteBoxCommand(val box:PBox) extends Command{
  lazy val parent = box.parent
  lazy val c = { 
    if (parent!=null){
      val s = for (w <- parent.connections;
          if ((box.ports contains w.from) || (box.ports contains w.to)))
            yield new DeleteWireCommand(w,parent)
      new ComposedCommand(List() ++ s)
    }else 
      new ComposedCommand(List())
  }
  def redo {
    c.redo
    parent.boxes -= box
    box.parent = null
    parent.notifyObservers
  }

  def undo {
    box.parent=parent
    parent.boxes += box
    c.undo
    parent.notifyObservers
  }
}
case class CreatePortCommand(box : PBox, slot: Slot) extends Command{
  val p = new PPort()
  p.slot = slot
  p.in = slot.left
  def redo {
    box.ports += p
    p.vbox = box
    box.notifyObservers
  }
  def undo {
    box.ports -= p
    p.vbox = null
    box.notifyObservers
  }
}
case class CreateWireCommand(parent: ComposedPBox, source:PPort, target:PPort) extends Command{
  val w = new PWire()
  w.from = source
  w.to = target
  def notifyObservers{
    parent.notifyObservers
    w.from .notifyObservers
    w.to .notifyObservers    
  }
  def redo {
    parent.connections += w
    notifyObservers
  }
  def undo {
    parent.connections -= w
    notifyObservers
  }
}
case class CreateBendpointCommand(wire : PWire, pos: Bendpoint, i : Int) extends Command{
  val old = wire.bendpoints
  def redo {
    val s = wire.bendpoints.splitAt(i) match {
      case (p1,p2) => p1 ::: pos :: p2
    }
    wire.bendpoints = s
    wire.notifyObservers
  }
  def undo {
    wire.bendpoints = old
    wire.notifyObservers
  }
}
case class DeleteBendpointCommand(wire : PWire, i:Int) extends Command{
  val old = wire.bendpoints
  def redo {
    wire.bendpoints =  wire.bendpoints.splitAt(i) match { 
      case (pre, e :: post)  => pre ::: post
      case _ => error("out of range")
    }
    wire.notifyObservers
  }
  def undo {
    wire.bendpoints = old
    wire.notifyObservers
  }
}
import scala.collection.JavaConversions._
case class PasteCommand(val c: ComposedPBox, val clip:serial.ModelProtos.Clipboard) extends Command{
  val boxes = clip.getBoxList map {b=>Deserialize.deserialize(b)}
  val wires = clip.getWireList map {w=>Deserialize.wire(boxes,c.ports,w)}
  val command = ComposedCommand(List() ++
      (boxes map (b=>CreateBoxCommand(c,b))) ++  
      (wires map (w=>CreateWireCommand(c,w.from,w.to))))
  def redo() = command.redo 
  def undo() = command.undo
}
case class CutCommand(val c: ComposedPBox, val boxes:Iterable[PBox], val wires:Iterable[PWire]) extends Command{
  val command = ComposedCommand(List() ++
      (wires map (w=>DeleteWireCommand(w,c))) ++  
      (boxes map (b=>DeleteBoxCommand(b))))
  def redo() = command.redo 
  def undo() = command.undo
}
