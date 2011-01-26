package org.zaluum.nide.zge
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.model._

class Viewer(parent: Composite, controller: Controller) extends AbstractViewer(parent, controller) {
  /*TOOLS*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.bcp)
  val palette = new Palette(this, parent.getShell, controller.bcp)
  var tool = new BoxTool(this)
  /*MODEL*/
  lazy val modelView = new ModelView(this,controller.model,controller.bcp)

  def model = controller.model
  override def dispose() {
    super.dispose()
    imageFactory.reg.dispose
  }
}

class ModelView(viewer: Viewer, val model: Model, val bcp: BoxClassPath) extends AbstractModelView(viewer) {

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
}