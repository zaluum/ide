package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{ Figure, FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, ColorConstants, RectangleFigure, Graphics }
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{ Composite, MessageBox }
import org.zaluum.nide.model._
import scala.collection.JavaConversions._

abstract class ItemViewer(parent: Composite, controller: Controller) extends Viewer(parent, controller) with Layers{
  /*SWT*/
  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val layer = new FreeformLayer {
    override def paintFigure(graphics: Graphics) {
      graphics.fillRectangle(getBounds());
      for (i ← 0 to getBounds.width by 15; j ← 0 to getBounds.height by 15) {
        graphics.drawPoint(i, j);
      }
    }
  }

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
    this.setContents(innerLayers);
    canvas.setViewport(this)
    marquee.setFill(false)
    marquee.setLineStyle(SWT.LINE_DASH);
    //UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  }
  def clear() {
    layer.removeAll()
    feedbackLayer.removeAll()
    portsLayer.removeAll()
    connectionsLayer.removeAll()
  }
  /*DEFS*/
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r: Rectangle) { marquee.setBounds(r) }
  def hideMarquee() { feedbackLayer.remove(marquee) }
  def executeOrNotify(cmd: TreeCommand) = {
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

  val selected = new SelectionManager()

  def deselectAll() { selected.deselectAll }
}
