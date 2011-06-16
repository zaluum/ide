package org.zaluum.nide.zge

import org.eclipse.draw2d.PositionConstants
import org.eclipse.draw2d.Triangle
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.events.{ FocusListener, FocusEvent }
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.jface.viewers.ICellEditorListener
import org.eclipse.draw2d.text.FlowPage
import org.eclipse.draw2d.text.TextFlow
import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.GroupBoxBorder
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.LayeredPane
import org.eclipse.draw2d.Layer
import org.eclipse.draw2d.LineBorder
import draw2dConversions._
import org.eclipse.draw2d.{ FreeformLayer, Ellipse, ColorConstants, Figure, ImageFigure, Polyline, ScalableFreeformLayeredPane, IFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler.{ Point ⇒ MPoint, _ }
import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._
import RichFigure._

trait ContainerItem extends Item {
  def viewer: Viewer
  def viewerResources: ViewerResources
  def layer: Figure
  def background: Figure
  def connectionsLayer: Figure
  def pointsLayer: Figure
  def portsLayer: Figure
  def feedbackLayer: Figure
  protected def itemAtIn(container: Figure, p: Point, debug: Boolean = false): Option[Item] = container.findDeepAt(point(p), 0, debug) {
    case i: Item ⇒ i
  }
  def symbol: BoxTypeSymbol = boxDef.symbol.asInstanceOf[BoxTypeSymbol]
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
  private def portFigures = portsLayer.getChildren.collect { case p: PortFigure ⇒ p }
  def findPortFigure(boxName: Name, portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒
      p.fromSym match {
        case valSym: ValSymbol ⇒ (valSym.name == boxName && p.sym.name == portName && p.in == in)
        case _ ⇒ false
      }
    }
  }
  def findPortFigure(portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒ p.fromSym.isInstanceOf[BoxTypeSymbol] && p.sym.name == portName && p.in == in }
  }

  protected def createGraph: ConnectionGraph = {
    val portVertexs = portsLayer.getChildren collect { case port: PortFigure ⇒ new PortVertex(port.portKey, port.anchor) }
    val junctions = boxDef.junctions.collect { case j: Junction ⇒ (j -> new Joint(j.p)) }.toMap
    val nonExistingPortVertex = scala.collection.mutable.Map[PortKey, MissingPortVertex]()
    val emptyVertexs = Buffer[EmptyVertex]()
    val edges = boxDef.connections.map {
      case c: ConnectionDef ⇒
        def toVertex(t: Tree, start: Boolean): Vertex = {
          def pos = if (start) c.headPoint else c.lastPoint
          t match {
            case JunctionRef(name) ⇒
              junctions.view.collect { case (k, joint) if (k.name == name) ⇒ joint }.head
            case p: PortRef ⇒
              val key = PortKey.create(p)
              portVertexs.find { _.key == key }
                .getOrElse {
                  nonExistingPortVertex.getOrElseUpdate(key, new MissingPortVertex(key, pos))
                }
            case EmptyTree ⇒
              val e = new EmptyVertex(pos)
              emptyVertexs += e
              e
          }
        }
        (c -> new Edge(toVertex(c.a, true), toVertex(c.b, false), c.points, Some(c)).fixEnds)
    }.toMap
    new ConnectionGraphV(
      portVertexs.toSet ++
        junctions.values ++
        nonExistingPortVertex.values ++
        emptyVertexs,
      edges.values.toSet)
  }
  val boxes = Buffer[ValDefItem]()
  val labels = Buffer[LabelItem]()
  def boxDef: BoxDef
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
        case Some(t: ValDef) ⇒
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
    val news = boxDef.vals filterNot (remove contains) collect { case v: ValDef ⇒ v }
    news foreach { v ⇒
      val f = v.scope.lookupBoxTypeLocal(v.typeName) match {
        case Some(tpe) ⇒
          val o = new OpenBoxFigure(ContainerItem.this, viewer, viewerResources)
          o.updateOpenBox(v, Map())
          o
        case None ⇒
          val valf = if (v.tpe.name == Name(classOf[org.zaluum.basic.Direct].getName))
            new DirectValFigure(ContainerItem.this)
          else
            new ImageValFigure(ContainerItem.this)
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
    for (j ← boxDef.junctions.asInstanceOf[List[Junction]]) {
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
  val viewer: Viewer,
  val viewerResources: ViewerResources) extends Figure with ValDefItem with ResizableFeedback with ContainerItem with Transparent {
  // Item
  def myLayer = container.layer
  def size = valDef.size getOrElse Dimension(Tool.gridSize*16, Tool.gridSize*16)
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
  val portDecls = Buffer[OpenPortDeclFigure]()
  val portSymbols = Buffer[PortSymbolFigure]()
  override def useLocalCoordinates = true
  def boxDef = valDef.tpe.decl.asInstanceOf[BoxDef]
  def updateOpenBox(v: ValDef, changes: Map[Tree, Tree]) {
    updateValDef(v)
    updateContents(changes)
    showArrowsIfNotBigEnough
  }
  def blink(b: Boolean) {
    background.setBackgroundColor (if (b) OpenBoxFigure.backgroundBlink else OpenBoxFigure.backgroundNormal)
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
    boxDef.children foreach {
      _ match {
        case p @ PortDef(name, typeName, in, inPos, extPos) ⇒
          def newFig(left: Boolean) = {
            val f = new OpenPortDeclFigure(OpenBoxFigure.this)
            f.update(p, left)
            portDecls += f
            if (showing) f.show()
          }
          in match {
            case In ⇒ newFig(true)
            case Out ⇒ newFig(false)
            case Shift ⇒ newFig(true); newFig(false)
          }
        case _ ⇒
      }
    }
    portSymbols.foreach(_.hide)
    portSymbols.clear()
    // super ports
    boxDef.symbol match {
      case s: BoxTypeSymbol ⇒
        for (sup ← s.superSymbol; p ← sup.portsWithSuper.values) {
          p match {
            case p: PortSymbol ⇒
              val f = new PortSymbolFigure(p, OpenBoxFigure.this)
              f.update()
              portSymbols += f
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