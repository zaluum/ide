package org.zaluum.runtime

trait Command {
  def execute = redo
  def undo
  def redo
}
case class CreateBoxCommand(val parent:ComposedVBox, val box:VBox){
  def undo = {
    box.parent = null
    parent.boxes -= box
  }
  def redo = {
    box.parent = parent
    parent.boxes += box
  }
}
class ComposedCommand(val commands : List[Command]) extends Command{
  override def execute = commands foreach { _.execute }
  def redo = commands foreach {_.redo }
  def undo = commands.reverse foreach { _.undo }
}
case class DeleteWireCommand(val w : VWire) extends Command{
  def redo {}
  def undo {}
}
case class DeleteBoxCommand(val box:VBox) extends Command{
  lazy val parent = box.parent
  lazy val c = { 
    if (parent!=null){
      val s = for (w <- parent.connections;
          if ((box.ports contains w.from) || (box.ports contains w.to)))
            yield new DeleteWireCommand(w)
      new ComposedCommand(List() ++ s)
    }else 
      new ComposedCommand(List())
  }
  override def execute {
    c.execute
    parent.boxes -= box
    box.parent = null
    
  }
  def redo {
    c.redo
    parent.boxes -= box
    box.parent = null
  }

  def undo {
    box.parent=parent
    parent.boxes += box
    c.undo
  }
}