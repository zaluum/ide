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
class ConnectCommand(c: Connection, bcd: BoxClassDecl) extends Command {
  def redo() { bcd.connections += c }
  def undo() { bcd.connections -= c }
  def canExecute = {
    c.from.isDefined && c.to.isDefined &&
      // not repeated 
      !bcd.connections.exists(other ⇒ other.from == c.from && other.to == c.to)
  }
}
class ResizeCommand(resizable: Resizable, pos:Point,size: Dimension) extends Command{
  val oldSize = resizable.size
  val oldPos = resizable.pos
  def redo() { resizable.size = size; resizable.pos = pos }
  def undo() { resizable.size = oldSize; resizable.pos = oldPos }
  def canExecute = true
}
class CreateCommand(box: Box, bcd: BoxClassDecl) extends Command {
  def redo { bcd.boxes += box }
  def undo { bcd.boxes -= box }
  def canExecute = true
}
class CreatePortDeclCommand(portDecl: PortDecl, bcd: BoxClassDecl) extends Command {
  def redo { bcd.portDecls += portDecl }
  def undo { bcd.portDecls -= portDecl }
  def canExecute = true
}