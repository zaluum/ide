package org.zaluum.nide.zge

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Buffer
import org.eclipse.draw2d.PositionConstants.EAST
import org.eclipse.draw2d.PositionConstants.NORTH
import org.eclipse.draw2d.PositionConstants.SOUTH
import org.eclipse.draw2d.PositionConstants.WEST
import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.ImageFigure
import org.eclipse.draw2d.Layer
import org.eclipse.draw2d.LayeredPane
import org.eclipse.draw2d.LineBorder
import org.eclipse.draw2d.PositionConstants
import org.eclipse.draw2d.Triangle
import org.eclipse.jface.resource.ImageDescriptor
import org.zaluum.expr.Literal
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.ConnectionDef
import org.zaluum.nide.compiler.ConnectionEnd
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.IfExprType
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Junction
import org.zaluum.nide.compiler.JunctionRef
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.{ Point ⇒ MPoint }
import org.zaluum.nide.compiler.PortRef
import org.zaluum.nide.compiler.PortSide
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.compiler.Vector2
import draw2dConversions._
import RichFigure._
import org.zaluum.nide.compiler.LiteralExprType
import org.zaluum.nide.compiler.Expressions

trait ContainerItem extends Item {
  def viewer: Viewer
  def layer: Figure
  def background: Figure
  def connectionsLayer: Figure
  def pointsLayer: Figure
  def portsLayer: Figure
  def feedbackLayer: Figure
  protected def itemAtIn(container: Figure, p: Point, debug: Boolean = false): Option[Item] = container.findDeepAt(point(p), 0, debug) {
    case i: Item ⇒ i
  }
  def itemAt(p: Point, debug: Boolean = false) = {
    itemAtIn(portsLayer, p, debug)
      .orElse(itemAtIn(layer, p, debug))
      .orElse(itemAtIn(connectionsLayer, p, debug))
  }
  def shallowItems = {
    (portsLayer.getChildren.view ++
      layer.getChildren.view ++
      connectionsLayer.getChildren.view ++
      pointsLayer.getChildren.view).collect { case i: Item ⇒ i }
  }
  protected def createGraph: ConnectionGraph = {
    val portVertexs = portsLayer.getChildren collect {
      case port: PortFigure ⇒
        new PortVertex(port.ps, port.anchor)
    }
    val junctions = block.junctions.collect { case j: Junction ⇒ (j -> new Joint(j.p)) }.toMap
    val emptyVertexs = Buffer[EmptyVertex]()

    val edges = block.connections.map {
      case c: ConnectionDef ⇒
        def toVertex(t: Option[ConnectionEnd], start: Boolean): Vertex = {
              def pos = if (start) c.headPoint else c.lastPoint
            t match {
              case Some(JunctionRef(name)) ⇒
                junctions.view.collect { case (k, joint) if (k.name == name) ⇒ joint }.head
              case Some(p: PortRef) ⇒
                portVertexs.find(_.ps == p.sym).get // BUG not found
              case None ⇒
                val e = new EmptyVertex(pos)
                emptyVertexs += e
                e
            }
          }
        val bad = block.sym.connections.isBad(c)
        (c -> new Edge(toVertex(c.a, true), toVertex(c.b, false), c.points, Some(c), bad).fixEnds)
    }.toMap
    new ConnectionGraphV(
      portVertexs.toSet ++
        junctions.values ++
        emptyVertexs,
      edges.values.toSet)
  }
  val boxes = Buffer[ValDefItem]()
  val labels = Buffer[LabelItem]()
  def block: Block
  def symbol = block.sym
  def templateSym = symbol.template
  def template = templateSym match {
    case v: ValSymbol     ⇒ v.tdecl.template.get
    case b: BoxTypeSymbol ⇒ b.tdecl.template
  }
  val junctions = Buffer[PointFigure]()
  val connections = Buffer[ConnectionFigure]()
  var graph: ConnectionGraph = _
  def updateContents(changes: Map[Tree, Tree]) {
    updateBoxes(changes)
    updatePorts(changes)
    updateJunctions()
    graph = createGraph
    updateConnections()
  }
  def updateBoxes(changes: Map[Tree, Tree]) {
    val remove = Buffer[ValDefItem]()
    for (bf ← boxes ++ labels) {
      changes.get(bf.valDef) match {
        case Some(t: ValDef) if (t.sym.isExecutable) ⇒
          bf match {
            case o: OpenBoxFigure ⇒ o.updateOpenBox(t, changes)
            case l: LabelItem ⇒
              t.label match {
                case Some(ld) ⇒
                  l.updateValDef(t)
                case None ⇒
                  l.hide()
                  remove += l
              }
            case s: ValDefItem ⇒ s.updateValDef(t)
          }
        case _ ⇒
          bf.hide
          remove += bf
      }
    }
    boxes.filterNot(remove.contains)
    labels.filterNot(remove.contains)
    val news = block.valDefs filterNot (remove.contains(_)) filter { _.sym.isExecutable }
    news foreach { v ⇒
      val f = v.template match {
        case Some(t) ⇒
          val o = v.sym.tpe match {
            case IfExprType ⇒ new IfOpenBoxFigure(ContainerItem.this, viewer)
            case _          ⇒ new OpenBoxFigure(ContainerItem.this, viewer)
          }
          o.updateOpenBox(v, Map())
          o
        case None ⇒
          val valf = v.tpe.fqName match {
            case LiteralExprType.fqName ⇒
              new LiteralFigure(ContainerItem.this)
            case name if (Expressions.thisFigureExpressions.contains(name)) ⇒
              new ThisOpValFigure(ContainerItem.this)
            case _ ⇒
              new ImageValFigure(ContainerItem.this)
          }
          valf.updateValDef(v)
          valf
      }
      if (showing) f.show
      boxes += f
    }
    news foreach { v ⇒
      v.label foreach { _ ⇒
        val l = new LabelItem(ContainerItem.this)
        if (showing) l.show
        l.updateValDef(v)
        labels += l
      }
    }
  }
  def updatePorts(changes: Map[Tree, Tree])
  def updateJunctions() {
    junctions.foreach { this.pointsLayer.safeRemove(_) }
    junctions.clear
    for (j ← block.junctions.asInstanceOf[List[Junction]]) {
      val p = new PointFigure
      p.update(j.p, j.tpe)
      junctions += p
    }
    if (showing) junctions.foreach { this.pointsLayer.add(_) }
  }
  def updateConnections() {
    connections.foreach { _.hide }
    connections.clear
    connections ++= graph.edges map { e ⇒ new ConnectionFigure(e, ContainerItem.this) }
    if (showing) connections.foreach { _.show }
  }
}
object OpenBoxFigure {
  val backgroundNormal = ColorConstants.white
  val backgroundBlink = ColorConstants.lightGray
}
class OpenBoxFigure(
    val container: ContainerItem,
    val viewer: Viewer) extends ValDefItem with ResizableFeedback with ContainerItem with Transparent {
  // Item
  def myLayer = container.layer
  def size = valDef.size getOrElse Dimension(Tool.gridSize * 16, Tool.gridSize * 16)
  // layers
  val inners = new LayeredPane
  val layer = new Layer
  val portsLayer = new Layer
  val connectionsLayer = new Layer
  val pointsLayer = new Layer
  val feedbackLayer = new Layer
  val background = new Layer
  // ContainerItem
  background.setBackgroundColor(ColorConstants.white)
  def helpers = portDecls ++ portSymbols
  val portDecls = Buffer[OpenPortBaseFigure]()
  val portSymbols = Buffer[PortSymbolFigure]()
  override def useLocalCoordinates = true
  def block = valDef.sym.currentBlock.tdecl
  def updateOpenBox(v: ValDef, changes: Map[Tree, Tree]) {
    updateValDef(v)
    updateContents(changes)
    showArrowsIfNotBigEnough
  }
  def blink(b: Boolean) {
    background.setBackgroundColor(if (b) OpenBoxFigure.backgroundBlink else OpenBoxFigure.backgroundNormal)
  }
  import PositionConstants._
  private def newTriangle(pos: Int) = {
    val t = new Triangle
    t.setBackgroundColor(ColorConstants.lightGray)
    val size = (14, 80)
    val s = if (pos == EAST || pos == WEST) size else size.swap
    t.setSize(s._1, s._2)
    t.setFillXOR(true)
    t.setDirection(pos)
    t.setOrientation(if (pos == EAST || pos == WEST) PositionConstants.VERTICAL else PositionConstants.HORIZONTAL)
    t
  }
  val triangles = Map(
    EAST -> newTriangle(EAST),
    WEST -> newTriangle(WEST),
    NORTH -> newTriangle(NORTH),
    SOUTH -> newTriangle(SOUTH))

  def showArrowsIfNotBigEnough() {
    val b = new Rectangle()
    inners.deepChildren.foreach { f ⇒ b.union(f.getBounds()) }
      def showTriangle(pos: Int) = {
        val t = triangles(pos)
        if (!container.feedbackLayer.getChildren.contains(t))
          add(t)
        val point = if (pos == EAST || pos == WEST) {
          val y = size.h / 2 - t.getSize.height / 2
          val x = if (pos == EAST) size.w - t.getSize.width - getInsets.left - getInsets.right
          else getInsets.left
          new Point(x, y)
        } else {
          val x = size.w / 2 - t.getSize.width / 2
          val y = if (pos == SOUTH) size.h - t.getSize.height - getInsets.top - getInsets.bottom
          else getInsets.top
          new Point(x, y)
        }
        t.setLocation(point)
      }
    triangles.values.foreach { this.safeRemove(_) }
    if (b.width > getClientArea.getSize.width) showTriangle(EAST)
    if (b.height > getClientArea.getSize.height) showTriangle(SOUTH)
    if (b.x < 0) showTriangle(WEST)
    if (b.y < 0) showTriangle(NORTH)
  }
  def updateMe() {}
  def updateValPorts() {}
  override def show() {
    super.show()
    portDecls.foreach { _.show }
    portSymbols.foreach { _.show }
  }
  override def hide() {
    super.hide()
    portDecls.foreach { _.hide }
    portSymbols.foreach { _.hide }
  }
  def updatePorts(changes: Map[Tree, Tree]) {
    portDecls.foreach { _.hide() }
    portDecls.clear()
    portSymbols.foreach(_.hide)
    portSymbols.clear()
    val vs = valDef.sym
    vs.portSides filter { _.fromInside } foreach {
      intPs ⇒ // TODO cleanup
          def extPs = vs.portSides.find(c ⇒ c.name == intPs.name && c.inPort == intPs.inPort && !c.fromInside).get
          def newFig(left: Boolean) = {
            val f = new OpenPortDeclFigure(OpenBoxFigure.this)
            f.update(intPs, extPs, left)
            portDecls += f
            if (showing) f.show()
          }
        if (intPs.pi.hasDecl) {
          intPs.pi.dir match {
            case In    ⇒ newFig(true)
            case Out   ⇒ newFig(false)
            case Shift ⇒ newFig(intPs.inPort);
          }
        } else {
          if (intPs.pi.dir == Out) {
            val f = new PortSymbolFigure(intPs, OpenBoxFigure.this)
            f.update
            portSymbols += f
            if (showing) f.show()
          } else if (intPs.pi.dir == In) {
            val f = new OpenPortFixedFigure(OpenBoxFigure.this)
            f.update(intPs, extPs, true, In, MPoint(0, 10))
            portDecls += f
            if (showing) f.show()
          }
        }
    }
  }
  override def paintClientArea(graphics: Graphics) {
    val rect = new Rectangle
    if (useLocalCoordinates) {
      graphics.translate(
        getBounds().x + getInsets().left,
        getBounds().y + getInsets().top);
    }
    graphics.clipRect(getClientArea(rect));
    graphics.pushState();
    graphics.setAlpha(25);
    graphics.setBackgroundColor(ColorConstants.lightGray);
    graphics.fillRectangle(rect);
    graphics.popState();
    graphics.restoreState();
    super.paintClientArea(graphics)
  }
  override def updateSize() {
    super.updateSize()
    inners.setSize(getBounds.getSize)
  }
  inners.add(background)
  inners.add(layer)
  inners.add(connectionsLayer)
  inners.add(portsLayer)
  inners.add(pointsLayer)
  inners.add(feedbackLayer)
  add(inners);
  setBorder(new LineBorder(ColorConstants.gray, Tool.gridSize))
}
abstract class Button(val openBox: OpenBoxFigure) extends ImageFigure with OverlappedItem with RectFeedback {
  var size = Dimension(10, 10)
  def imageFactory = container.viewer.zproject.imageFactory
  def myLayer = container.layer
  def blink(b: Boolean) {}
  def extPos = MPoint(16, 0)
  def constantDisplacement = Vector2(0, -size.h + openBox.getInsets().top)
  def imageDesc: ImageDescriptor
  var currentDesc: Option[ImageDescriptor] = None
  def update() {
      def loadImage() {
        currentDesc = Some(imageDesc)
        val img = imageFactory.image(imageDesc)
        setImage(img)
        size = Dimension(img.getBounds.width, img.getBounds().height)
      }
    currentDesc match {
      case Some(c) if (c != imageDesc) ⇒
        imageFactory.destroy(c)
        loadImage()
      case None ⇒ loadImage()
      case _    ⇒
    }
    updateSize()
  }
  override def hide() {
    super.hide()
    currentDesc foreach { imageFactory.destroy }
    currentDesc = None
  }
}
class IfOpenBoxFigure(container: ContainerItem, viewer: Viewer) extends OpenBoxFigure(container, viewer) {
  val buttons = Buffer[Button]()
  buttons += new Button(this) {
    def imageDesc =
      if (openBox.symbol.blockNumeral == 0)
        imageFactory.buttonIfTrue
      else imageFactory.buttonIfFalse
  }
  override def show() {
    super.show()
    buttons.foreach { _.show }
  }
  override def hide() {
    super.hide()
    buttons.foreach { _.hide }
  }
  override def updateMe {
    buttons.foreach { _.update() }
  }
}
