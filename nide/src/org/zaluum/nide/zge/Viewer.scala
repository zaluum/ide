package org.zaluum.nide.zge

import org.zaluum.nide.compiler.BoxClassPath
import org.eclipse.draw2d.Polyline
import org.eclipse.swt.graphics.Image
import org.eclipse.draw2d.ImageFigure
import org.eclipse.draw2d.GridLayout
import org.eclipse.draw2d.FreeformLayout
import org.eclipse.draw2d.XYLayout
import org.eclipse.draw2d.FlowLayout
import scala.collection.mutable.Buffer
import javax.swing.UIManager
import javax.swing.JComponent
import javax.swing.JButton
import javax.swing.JPanel
import org.eclipse.swt.SWT
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.IFigure
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, FreeformViewport, LightweightSystem, Ellipse, ColorConstants, Figure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point, Dimension }
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.model._

class Viewer(parent: Composite, val controller: Controller) {

  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)

  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val layer = new FreeformLayer
  layer.setOpaque(true);
  layer.setBackgroundColor(ColorConstants.white)
  //layer.setLayoutManager(new FreeformLayout)
  val viewport = new FreeformViewport();
  val innerLayers = new ScalableFreeformLayeredPane();
  innerLayers.add(layer)
  innerLayers.add(portsLayer)
  innerLayers.add(connectionsLayer)
  innerLayers.add(feedbackLayer)
  viewport.setContents(innerLayers);
  canvas.setViewport(viewport)
  var tool = new MoveTool(this)
  val modelView = controller.registerView(this)
  def dispose() {
    canvas.dispose()
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
  def figureAt(p: Point) = findShallowAt(layer, p) map { case (bf: BoxFigure) ⇒ bf }
  def feedbackAt(p: Point) = findDeepAt(feedbackLayer, p)

  val marquee = new RectangleFigure
  marquee.setFill(false)
  marquee.setLineStyle(SWT.LINE_DASH);
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r: Rectangle) { marquee.setBounds(r) }
  def hideMarquee() { feedbackLayer.remove(marquee) }
  UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  object imageFactory {
    lazy val notFound = new Image(canvas.getDisplay, "icons/notFound.png")
    def apply(str: String) = {
      try {
        new Image(canvas.getDisplay, str)
      } catch {
        case _ ⇒ notFound
      }
    }
  }
}
class ModelView(val viewer: Viewer, val model: Model, val bcp: BoxClassPath) {

  var selectedBoxes = Set[BoxFigure]()
  private var viewMap = Map[Box, BoxFigure]()
  private var connectionMap = Map[Connection, ConnectionFigure]()
  def portFigure(p: Port): Option[PortFigure] = {
    viewMap.get(p.box) flatMap {
      _ match {
        case w: WithPorts ⇒  w.ports.get(p)
        case _ ⇒ None
      }
    }
  }
  def update() {
    def updateBoxes() {
      val removed = viewMap.keySet -- model.boxes
      val added = model.boxes -- viewMap.keys
      removed foreach { b ⇒ viewMap(b).hide; viewMap -= b }
      for (box ← added) {
        //val boxview = new SwingBoxFigure(viewer,box, new JButton("text"))
        val cl = bcp.find(box.className)
        val image = cl map { c ⇒ viewer.imageFactory(c.image) }
        val boxview = new ImageBoxFigure(box, cl, viewer, image.getOrElse { viewer.imageFactory.notFound })
        viewMap += (box -> boxview)
        boxview.show
      }
      viewMap.values foreach { _.update() }
    }
    def updateConnections() {
      val removed = connectionMap.keySet -- model.connections
      val added = model.connections -- connectionMap.keySet
      removed foreach { c ⇒ connectionMap(c).hide; connectionMap -= c }
      for (conn ← added) {
        val conFig = new ConnectionFigure(conn, this)
        connectionMap += (conn -> conFig)
        conFig.show
      }
      connectionMap.values foreach { _.update() }
    }
    updateBoxes()
    updateConnections
    // connections 
  }
  def select(box: BoxFigure) {
    if (!selectedBoxes(box)) {
      selectedBoxes += box
      box.showFeedback
    }
  }
  def deselect(box: BoxFigure) {
    if (selectedBoxes(box)) {
      box.hideFeedback
      selectedBoxes -= box
    }
  }
  def deselectAll() {
    selectedBoxes foreach { deselect(_) }
  }
  def toggleSelection(box: BoxFigure) {
    if (selectedBoxes(box)) deselect(box)
    else select(box)
  }
  def updateSelection(figs: Set[BoxFigure], shift: Boolean) {
    if (shift) {
      figs foreach { toggleSelection(_) }
    } else {
      deselectAll()
      figs foreach { select(_) }
    }
  }
}