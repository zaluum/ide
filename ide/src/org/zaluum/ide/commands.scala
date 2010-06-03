package org.zaluum.ide
import org.zaluum.runtime._
import org.eclipse.gef.commands.Command
class SCommand[T](val old: T, val set:(T=>Unit),val news:T) extends Command{
  override def execute = redo
  override def redo = set(news)
  override def undo = set(old)
}