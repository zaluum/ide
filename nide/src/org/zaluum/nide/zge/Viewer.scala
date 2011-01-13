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
  def figureAt(p: Point) = findShallowAt(layer, p) map { case (bf: BasicFigure) ⇒ bf }
  def feedbackAt(p: Point) = findDeepAt(feedbackLayer, p)
  def lineAt(p: Point) = findDeepAt(connectionsLayer, p) map { case l: LineFigure ⇒ l }
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

  var selected = new SelectionManager[BasicFigure]()
  var selectedLines = new SelectionManager[LineFigure]()
  object boxMapper extends ModelViewMapper[Box, BoxFigure] {
    def modelSet = model.boxes
    def buildFigure(box: Box) = {
      val cl = bcp.find(box.className)
      val image = cl map { c ⇒ viewer.imageFactory(c.image) }
      new ImageBoxFigure(box, cl, viewer, image.getOrElse { viewer.imageFactory.notFound })
    }
  }
  object connectionMapper extends ModelViewMapper[Connection, ConnectionFigure] {
    def modelSet =  model.connections 
    def buildFigure(conn: Connection) = new ConnectionFigure(conn, ModelView.this)
  }
  object portDeclMapper extends ModelViewMapper[PortDecl, PortDeclFigure] {
    def modelSet = model.portDecls 
    def buildFigure(portDecl: PortDecl) = new PortDeclFigure(portDecl,viewer,Some(TypedPort("D", false, portDecl.name, (0,0)))) // FIXME types
  }
  def classOfB(b: Box) = bcp.find(b.className)
  
  def findPortFigure(portRef: PortRef) = {
    if (portRef.box==null) { // FIXME 
      portDeclMapper.values find 
        { _.portDecl.name == portRef.name} map 
        { _.portMapper.values.head }
    }else {
      boxMapper.get(portRef.box) match {
        case Some(w: FigureWithPorts) ⇒ w.find(portRef.name)
        case _ ⇒ None
      } 
    }
  }
  
  def update() {
    boxMapper.update()
    portDeclMapper.update()
    connectionMapper.update()
  }
  def deselectAll() {
    selected.deselectAll
    selectedLines.deselectAll
  }
}