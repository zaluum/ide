package org.zaluum.nide.zge
import javax.swing.UIManager
import org.eclipse.draw2d.{ FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, FreeformViewport, LightweightSystem, ColorConstants, Figure, IFigure, RectangleFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point }
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Cursor
import org.eclipse.swt.widgets.{ Composite, MessageBox }
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }

class Viewer(parent: Composite, val controller: Controller) {
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.bcp)
  def shell = parent.getShell
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
  // SHELL
  val palette = new Palette(this, parent.getShell)
  var tool = new MoveTool(this)
  val modelView = controller.registerView(this)
  def model = controller.model
  def dispose() {
    canvas.dispose()
    imageFactory.reg.dispose
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
}

class ModelView(val viewer: Viewer, val model: Model, val bcp: BoxClassPath) {

  var selected = new SelectionManager[BasicFigure]()
  var selectedLines = new SelectionManager[LineFigure]()
  object boxMapper extends ModelViewMapper[Box, BoxFigure] {
    def modelSet = model.boxes
    def buildFigure(box: Box) = {
      val cl = bcp.find(box.className)
      new ImageBoxFigure(box, cl, viewer)
    }
  }
  object connectionMapper extends ModelViewMapper[Connection, ConnectionFigure] {
    def modelSet = model.connections
    def buildFigure(conn: Connection) = new ConnectionFigure(conn, ModelView.this)
  }
  object portDeclMapper extends ModelViewMapper[PortDecl, PortDeclFigure] {
    def modelSet = model.portDecls
    def buildFigure(portDecl: PortDecl) = new PortDeclFigure(portDecl, viewer) // FIXME types
  }
  def classOfB(b: Box) = bcp.find(b.className)

  def findPortFigure(portRef: PortRef): Option[PortFigure] = portRef match {
    case b: BoxPortRef ⇒
      boxMapper.get(b.box) flatMap { _.find(b.name) }
    case m: ModelPortRef ⇒
      portDeclMapper.values find { _.portDecl.name == m.name } map
        { _.portMapper.values.head }
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