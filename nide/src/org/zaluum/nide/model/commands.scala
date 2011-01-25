package org.zaluum.nide.model

abstract class Command {
  def act() = redo()
  def redo()
  def undo()
  def canExecute: Boolean
}
class ChainCommand(val commands: List[Command]) extends Command {
  override def act() { commands.foreach { _.act() } }
  def redo() { commands.foreach { _.redo() } }
  def undo() { commands.reverse.foreach { _.undo() } }
  def canExecute = commands.forall(_.canExecute)
}
class MoveCommand(positionable: Positionable, pos: Point) extends Command {
  var old = positionable.pos
  def redo() { positionable.pos = pos }
  def undo() { positionable.pos = old }
  def canExecute = true
}
class ConnectCommand(m: Model, c: Connection) extends Command {
  def redo() { m.connections += c }
  def undo() { m.connections -= c }
  def canExecute = {
    c.from.isDefined && c.to.isDefined &&
      // not repeated 
      !m.connections.exists(other ⇒ other.from == c.from && other.to == c.to)
  }
}
class ResizeCommand(resizable: Resizable, pos:Point,size: Dimension) extends Command{
  val oldSize = resizable.size
  val oldPos = resizable.pos
  def redo() { resizable.size = size; resizable.pos = pos }
  def undo() { resizable.size = oldSize; resizable.pos = oldPos }
  def canExecute = true
}
class CreateCommand(box: Box, model: Model) extends Command {
  def redo { model.boxes += box }
  def undo { model.boxes -= box }
  def canExecute = true
}
class CreatePortDeclCommand(portDecl: PortDecl, model: Model) extends Command {
  def redo { model.portDecls += portDecl }
  def undo { model.portDecls -= portDecl }
  def canExecute = true
}