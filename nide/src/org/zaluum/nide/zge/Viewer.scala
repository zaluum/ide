package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{FigureCanvas, FreeformViewport, LightweightSystem}
import org.eclipse.swt.graphics.Cursor
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.model._
import scala.collection.JavaConversions._

abstract class Viewer(parent: Composite, val controller: Controller) extends FreeformViewport {
  def shell = parent.getShell
  def display = shell.getDisplay
  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  def tool: Tool
  def refresh()
  def dispose() {
    canvas.dispose()
    controller.unregisterViewer(this)
  }
}
