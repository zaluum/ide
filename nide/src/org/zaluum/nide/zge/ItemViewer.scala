package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.FigureCanvas
import org.eclipse.draw2d.FreeformLayer
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.ScalableFreeformLayeredPane
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DropTarget
import org.eclipse.swt.dnd.DropTargetAdapter
import org.eclipse.swt.dnd.DropTargetEvent
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Tree

abstract class ItemViewer(parent: Composite, controller: Controller)
    extends Viewer(parent, controller) with ContainerItem with PropertySource {
  /*SWT*/
  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val pointsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val background = new FreeformLayer /* {
    override def paintFigure(g:Graphics) { DotPainter.dotFill(g,getBounds) }
  }*/
  val layer = new FreeformLayer
  val innerLayers = new ScalableFreeformLayeredPane()
  val marquee = new RectangleFigure;
  {
    canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)
    val dt = new DropTarget(viewer.canvas, DND.DROP_MOVE);
    dt.setTransfer(Array(TextTransfer.getInstance()));
    dt.addDropListener(new DropTargetAdapter() {
      override def dragEnter(event: DropTargetEvent) {
        canvas.setFocus
      }
      override def drop(event: DropTargetEvent) {
        tool.handleDrop(event.x, event.y, event.data.asInstanceOf[String])
      }
    })
    background.setOpaque(true);
    background.setBackgroundColor(ColorConstants.white)
    background.setForegroundColor(ColorConstants.blue)
    innerLayers.add(background)
    innerLayers.add(layer)
    innerLayers.add(connectionsLayer)
    innerLayers.add(portsLayer)
    innerLayers.add(pointsLayer)
    innerLayers.add(feedbackLayer)
    this.setContents(innerLayers);
    canvas.setViewport(this)
    marquee.setFill(false)
    marquee.setLineStyle(SWT.LINE_DASH);
    //UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  }
  import RichFigure._
  override def dispose() {
    this.deepChildren foreach {
      _ match {
        case a: AutoDisposeImageFigure ⇒ a.disposeImage()
        case _                         ⇒
      }
    }
    super.dispose()
  }
  /*DEFS*/
  def blink(s: SelectionSubject) {
    this.deepChildren.find {
      case i: Item ⇒ i.selectionSubject == Some(s)
      case _       ⇒ false
    } foreach {
      case i: Item ⇒ i.blink();
    }
  }
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r: Rectangle) { marquee.setBounds(r) }
  def hideMarquee() { feedbackLayer.remove(marquee) }
  def selectedItems: Set[Item]
  val selection = new SelectionManager[SelectionSubject]()
  def blink(b: Boolean) {}

}
