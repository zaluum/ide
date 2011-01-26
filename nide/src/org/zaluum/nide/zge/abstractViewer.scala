package org.zaluum.nide.zge

import org.eclipse.draw2d.Graphics
import scala.collection.mutable.Buffer
import draw2dConversions._
import javax.swing.UIManager
import org.eclipse.draw2d.{ Figure, FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, FreeformViewport, LightweightSystem, ColorConstants, IFigure, RectangleFigure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Cursor
import org.eclipse.swt.widgets.{ Composite, MessageBox }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import scala.collection.JavaConversions._
import scala.collection.mutable.Stack

abstract class AbstractViewer(parent: Composite, val controller: Controller) {
  /*SWT*/
  def shell = parent.getShell
  def display = shell.getDisplay
  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val layer = new FreeformLayer {
    override def paintFigure(graphics:Graphics) {
      graphics.fillRectangle(getBounds());
      for (i<- 0 to getBounds.width by 15; j<- 0 to getBounds.height by 15) {
        graphics.drawPoint(i, j);
      }
    }
  }
  //layer.setLayoutManager(new FreeformLayout)
  val viewport = new FreeformViewport();
  val innerLayers = new ScalableFreeformLayeredPane()
  val marquee = new RectangleFigure;
  {
    canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)
    layer.setBackgroundColor(ColorConstants.white)
    layer.setForegroundColor(ColorConstants.blue)
    innerLayers.add(layer)
    innerLayers.add(portsLayer)
    innerLayers.add(connectionsLayer)
    innerLayers.add(feedbackLayer)
    viewport.setContents(innerLayers);
    canvas.setViewport(viewport)
    marquee.setFill(false)
    marquee.setLineStyle(SWT.LINE_DASH);
    //UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  }

  /*DEFS*/
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r: Rectangle) { marquee.setBounds(r) }
  def hideMarquee() { feedbackLayer.remove(marquee) }
  def executeOrNotify(cmd: Command) = {
    if (cmd.canExecute) {
      controller.exec(cmd)
      true
    } else {
      val msg = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK)
      msg.setMessage("Cannot be executed")
      msg.open
      false
    }
  }
  def dispose() {
    canvas.dispose()
    controller.unregisterView(modelView)
  }
  def setCursor(cursor: Cursor) {
    canvas.setCursor(cursor)
  }
  def findDeepAt(container: IFigure, p: Point) = {
    Option(container.findFigureAt(p.x, p.y)) filter (_ != container)
  }
  def findShallowAt(container: IFigure, p: Point) = {
    import scala.collection.JavaConversions._
    container.getChildren.asInstanceOf[java.util.List[IFigure]] find { _.containsPoint(p) };
  }
  def figureAt(p: Point) = findShallowAt(layer, p) map { case (bf: ItemFigure) ⇒ bf }
  def feedbackAt(p: Point) = findDeepAt(feedbackLayer, p)
  def lineAt(p: Point) = findDeepAt(connectionsLayer, p) map { case l: LineFigure ⇒ l }
  def modelView: AbstractModelView
  def tool: AbstractTool
  controller.registerView(modelView)
}
abstract class AbstractModelView(val viewer: AbstractViewer) {
  val selected = new SelectionManager()
  def deselectAll() { selected.deselectAll }
  def update()
}
