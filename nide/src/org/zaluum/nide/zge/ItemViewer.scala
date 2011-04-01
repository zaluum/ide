package org.zaluum.nide.zge

import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Tree
import draw2dConversions._
import org.eclipse.draw2d.{ Figure, FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, ColorConstants, RectangleFigure, Graphics }
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{ Composite, MessageBox }
import scala.collection.JavaConversions._

abstract class ItemViewer(parent: Composite, controller: Controller) extends Viewer(parent, controller) with Container {
  /*SWT*/
  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val pointsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val background = new FreeformLayer {
    override def paintFigure(g:Graphics) { DotPainter.dotFill(g,getBounds) }
  }
  val layer = new FreeformLayer

  val innerLayers = new ScalableFreeformLayeredPane()
  val marquee = new RectangleFigure;
  {
    canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)
    background.setBackgroundColor(ColorConstants.white)
    background.setForegroundColor(ColorConstants.blue)
    innerLayers.add(background)
    innerLayers.add(layer)
    innerLayers.add(portsLayer)
    innerLayers.add(connectionsLayer)
    innerLayers.add(pointsLayer)
    innerLayers.add(feedbackLayer)
    this.setContents(innerLayers);
    canvas.setViewport(this)
    marquee.setFill(false)
    marquee.setLineStyle(SWT.LINE_DASH);
    //UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  }
  /*DEFS*/
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r: Rectangle) { marquee.setBounds(r) }
  def hideMarquee() { feedbackLayer.remove(marquee) }
  def selectedItems: Set[Item]
  val selection = new SelectionManager[SelectionSubject]()
}
