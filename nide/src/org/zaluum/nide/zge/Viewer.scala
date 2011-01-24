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
  /*SWT*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.bcp)
  def shell = parent.getShell
  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val layer = new FreeformLayer
  //layer.setLayoutManager(new FreeformLayout)
  val viewport = new FreeformViewport();
  val innerLayers = new ScalableFreeformLayeredPane()
  val marquee = new RectangleFigure;
  {
    canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)
    layer.setOpaque(true);
    layer.setBackgroundColor(ColorConstants.white)
    innerLayers.add(layer)
    innerLayers.add(portsLayer)
    innerLayers.add(connectionsLayer)
    innerLayers.add(feedbackLayer)
    viewport.setContents(innerLayers);
    canvas.setViewport(viewport)
    marquee.setFill(false)
    marquee.setLineStyle(SWT.LINE_DASH);
    UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  }
  /*TOOLS*/
  val palette = new Palette(this, parent.getShell)
  var tool = new MoveTool(this)
  /*MODEL*/
  val modelView = controller.registerView(this)
  def model = controller.model
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
  def figureAt(p: Point) = findShallowAt(layer, p) map { case (bf: ItemFigure) ⇒ bf }
  def feedbackAt(p: Point) = findDeepAt(feedbackLayer, p)
  def lineAt(p: Point) = findDeepAt(connectionsLayer, p) map { case l: LineFigure ⇒ l }
}

class ModelView(val viewer: Viewer, val model: Model, val bcp: BoxClassPath) {

  var selected = new SelectionManager()
  def selectedBoxes = selected.selected collect { case x: BoxFigure ⇒ x.box }
  def selectedPorts = selected.selected collect { case x: PortDeclFigure ⇒ x.portDecl }
  def selectedConnections = selected.selected collect { case x: LineFigure ⇒ x.cf.c }

  def createRemoveCommand: Command = {
    val boxes = selectedBoxes
    val ports = selectedPorts
    val connections = selectedConnections
    new Command {
      var disconnectedFrom = Map[Connection, PortRef]()
      var disconnectedTo = Map[Connection, PortRef]()
      def redo() {
        model.boxes --= boxes
        model.portDecls --= ports
        model.connections --= connections
        disconnectedFrom = Map.empty
        disconnectedTo = Map.empty
        for (c ← model.connections) {
          def isDisconnected(pr: PortRef) = pr match {
            case br: BoxPortRef if (boxes.contains(br.box)) ⇒ true
            case mr: ModelPortRef if (ports exists (_.name == mr.name)) ⇒ true
            case _ ⇒ false
          }
          c.from foreach (pr ⇒ if (isDisconnected(pr)) {
            c.from = None
            disconnectedFrom += (c -> pr)
          })
          c.to foreach (pr ⇒ if (isDisconnected(pr)) {
            c.to = None
            disconnectedTo += (c -> pr)
          })
        }
        deselectAll
      }
      def canExecute = !(boxes.isEmpty && ports.isEmpty && connections.isEmpty)
      def undo() {
        model.boxes ++= boxes
        model.portDecls ++= ports
        model.connections ++= connections
        disconnectedFrom foreach { case (c, portRef) ⇒ c.from = Some(portRef) }
        disconnectedTo foreach { case (c, portRef) ⇒ c.to = Some(portRef) }
        deselectAll
      }
    }
  }
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
    def buildFigure(portDecl: PortDecl) = new PortDeclFigure(portDecl, viewer)
  }
  def gotoMarker(l: Location) {
    model.locate(l) foreach {
      deselectAll()
      _ match {
        case p: PortRef ⇒ findPortFigure(p) foreach { _.showFeedback }
        case c: Connection ⇒ findConnection(c) foreach { c ⇒ selected.select(c.lines.values.head) }
        case b: Box ⇒ findBox(b) foreach { bf ⇒ selected.select(bf) }
        case p: PortDecl ⇒ findPortDecl(p) foreach { pf ⇒ selected.select(pf) }
        case _ ⇒
      }
    }
  }
  def findConnection(conn: Connection): Option[ConnectionFigure] = connectionMapper.get(conn)
  def findBox(b: Box): Option[BoxFigure] = boxMapper.get(b)
  def findPortDecl(p: PortDecl): Option[PortDeclFigure] = portDeclMapper.get(p)
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
  }
}