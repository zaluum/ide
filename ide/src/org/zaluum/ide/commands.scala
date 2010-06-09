package org.zaluum.ide
import org.zaluum.runtime.{Command=>C,_}
import org.eclipse.gef.commands.Command
class SCommand[T](val old: T, val set:(T=>Unit),val news:T,val s:Subject) extends Command{
  override def execute = redo
  override def redo = { set(news); s.notifyObservers }
  override def undo = { set(old); s.notifyObservers }
}
class CommandWrap(val c:C) extends Command{
  override def execute =  c.redo
  override def redo= c.redo
  override def undo = c.undo
}
object Commands {
  implicit def commandWrap(c:C) : Command = new CommandWrap(c)
}