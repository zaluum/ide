package org.zaluum.runtime

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
case class CreateBoxCommand(val parent:ComposedVBox, val box:VBox) extends Command{
  def undo = {
    box.parent = null
    parent.boxes -= box
    parent.notifyObservers
  }
  def redo = {
    box.parent = parent
    parent.boxes += box
    parent.notifyObservers
  }
}
class ComposedCommand(val commands : List[Command]) extends Command{
  def redo = commands foreach {_.redo }
  def undo = commands.reverse foreach { _.undo }
}
case class DeleteWireCommand(val w : VWire, val p : ComposedVBox) extends Command{
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
case class DeleteBoxCommand(val box:VBox) extends Command{
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
case class CreatePortCommand(box : VBox, slot: Slot) extends Command{
  val p = new PPort()
  p.slot = slot
  def redo {
    box.asInstanceOf[PBox].ports += p
    p.vbox = box
    box.notifyObservers
  }
  def undo {
    box.asInstanceOf[PBox].ports -= p
    p.vbox = null
    box.notifyObservers
  }
}
case class CreateWireCommand(parent: ComposedVBox, source:VPort, target:VPort) extends Command{
  val w = new PWire()
  w.from = source
  w.to = target
  def notifyObservers{
    parent.notifyObservers
    w.from .notifyObservers
    w.to .notifyObservers    
  }
  def redo {
    parent.asInstanceOf[ComposedPBox].connections.add(w)
    notifyObservers
  }
  def undo {
    parent.asInstanceOf[ComposedPBox].connections.remove(w)
    notifyObservers
  }
}
case class CreateBendpointCommand(wire : VWire, pos: Bendpoint, i : Int) extends Command{
  val old = wire.bendpoints
  def redo {
    val s = wire.bendpoints.splitAt(i) match {
      case (p1,p2) => p1 ::: pos :: p2
    }
    wire.asInstanceOf[PWire].bendpoints = s
    wire.notifyObservers
  }
  def undo {
    wire.asInstanceOf[PWire].bendpoints = old
    wire.notifyObservers
  }
}
case class DeleteBendpointCommand(wire : VWire, i:Int) extends Command{
  val old = wire.bendpoints
  def redo {
    wire.asInstanceOf[PWire].bendpoints =  wire.bendpoints.splitAt(i) match { 
      case (pre, e :: post)  => pre ::: post
      case _ => error("out of range")
    }
    wire.notifyObservers
  }
  def undo {
    wire.asInstanceOf[PWire].bendpoints = old
    wire.notifyObservers
  }
}