package org.zaluum.nide.zge

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
  def feedbackLayer: Figure
  protected def itemAtIn(container: Figure, p: Point, debug: Boolean = false): Option[Item] = container.findDeepAt(point(p), 0, debug) {
    case i: Item ⇒ i
  }
  def symbol: BoxTypeSymbol = boxDef.symbol.asInstanceOf[BoxTypeSymbol]
  def connectionsLayer: Figure
  def pointsLayer: Figure
  def portsLayer: Figure
  def itemAt(p: Point, debug: Boolean = false) = {
    itemAtIn(portsLayer, p, debug)
      .orElse(itemAtIn(layer,p, debug))
      .orElse(itemAtIn(connectionsLayer, p, debug))
  }
  private def portFigures = portsLayer.getChildren.collect { case p: PortFigure ⇒ p }
  def findPortFigure(boxName: Name, portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒
      p.fromSym match {
        case valSym : ValSymbol ⇒ (valSym.name == boxName && p.sym.name == portName && p.in == in)
        case _ ⇒ false
      }
    }
  }
  def findPortFigure(portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒ p.fromSym.isInstanceOf[BoxTypeSymbol] && p.sym.name == portName && p.in == in }
  }
  protected def createGraph: ConnectionGraph = {
    // fixme portslayer
    val portVertexs = portsLayer.getChildren collect { case port: PortFigure ⇒ new PortVertex(port, port.anchor) }
    val junctions = boxDef.junctions.collect { case j: Junction ⇒ (j -> new Joint(j.p)) }.toMap
    val edges = boxDef.connections.map {
      case c: ConnectionDef ⇒
        def toVertex(t: Tree, start: Boolean): Vertex = t match {
          case JunctionRef(name) ⇒ junctions.collect { case (k, joint) if (k.name == name) ⇒ joint }.head
          case p: PortRef ⇒
            portVertexs.find { _.portPath == PortPath(p) }.getOrElse { throw new RuntimeException("could not find vertex for " + p + " " + PortPath(p)) }

        }
        (c -> new Edge(toVertex(c.a, true), toVertex(c.b, true), c.points, Some(c)).fixEnds)
    }.toMap
    new ConnectionGraphV(portVertexs.toSet ++ junctions.values, edges.values.toSet)
  }
  val boxes = Buffer[ValDefItem]()
  def boxDef :BoxDef 
  val junctions = Buffer[PointFigure]()
  val connections = Buffer[ConnectionFigure]()
  var graph : ConnectionGraph = _
  def updateContents(changes:Map[Tree,Tree]) {
    updateBoxes(changes)
    updatePorts(changes)
    updateJunctions()
    graph = createGraph
    updateConnections()
  }  
  def updateBoxes(changes: Map[Tree, Tree]) {
    val remove = Buffer[ValDefItem]()
    for (bf ← boxes) {
      changes.get(bf.valDef) match {
        case Some(t:ValDef) ⇒ 
          bf match {
            case o:OpenBoxFigure => o.updateOpenBox(t,changes)
            case s:ValDefItem => s.updateValDef(t)
          }
        case _ ⇒
          bf.hide
          remove += bf
      }
    }
    boxes.filterNot(remove.contains)
    val news = boxDef.vals filterNot (remove contains) collect { case v: ValDef ⇒ v }
    news foreach { v ⇒
      val f = v.scope.lookupBoxTypeLocal(v.typeName) match {
        case Some(tpe) ⇒
          val o = new OpenBoxFigure(ContainerItem.this,
            viewer,
            viewerResources)
          o.updateOpenBox(v,Map())
          o
        case None ⇒
          val f = v.params.headOption match {
            case Some(p: Param) ⇒
              new DirectValFigure(ContainerItem.this)
            case _ ⇒
              new ImageValFigure(ContainerItem.this)
          }
          f.updateValDef(v)
          f
      }
      if (showing) f.show
      boxes += f
    }
  }
  def updatePorts(changes : Map[Tree,Tree])
  def updateJunctions() {
    junctions.foreach { container.pointsLayer.safeRemove(_) }
    junctions.clear
    for (j ← boxDef.junctions.asInstanceOf[List[Junction]]) {
      val p = new PointFigure
      p.update(j.p,j.tpe)
      junctions += p
    }
    if (showing) junctions.foreach { container.pointsLayer.add(_) }
  }
  def updateConnections() {
    connections.foreach { _.hide }
    connections.clear
    connections ++= graph.edges map { e ⇒ new ConnectionFigure(e, ContainerItem.this) }
    if (showing) connections.foreach { _.show }
  }
}

class OpenBoxFigure(
    val container: ContainerItem,
    val viewer: Viewer,
    val viewerResources: ViewerResources) extends Figure with ValDefItem with ResizableFeedback with ContainerItem with Transparent {
  // Item
  def myLayer = container.layer
  def size = valDef.size getOrElse Dimension(100, 100)
  // layers
  val inners = new LayeredPane
  val layer = new Layer
  val portsLayer = new Layer
  val connectionsLayer = new Layer
  val pointsLayer = new Layer
  val feedbackLayer = new Layer
  val background = new Layer
  // ContainerItem
  def helpers =  portDecls ++ portSymbols
  val portDecls = Buffer[OpenPortDeclFigure]()
  val portSymbols = Buffer[PortSymbolFigure]()
  override def useLocalCoordinates = true
  def boxDef = valDef.tpe.decl.asInstanceOf[BoxDef]
  def updateOpenBox(v:ValDef,changes:Map[Tree,Tree]) {
    updateValDef(v)
    updateContents(changes)
  }
  def updateMe() {}
  def updateValPorts(){}
  override def show(){
    super.show()
    portDecls.foreach {_.show}
    portSymbols.foreach {_.show}
  }
  override def hide(){
    super.hide()
    portDecls.foreach {_.hide}
    portSymbols.foreach {_.hide}
  }
  def updatePorts(changes : Map[Tree,Tree]) {
    portDecls.foreach {_.hide()}
    portDecls.clear()
    boxDef.children foreach {
      _ match {
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
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
        for (sup ← s.superSymbol; p ← sup.ports.values) {
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
  inners.add(pointsLayer)
  inners.add(portsLayer)
  inners.add(connectionsLayer)
  inners.add(feedbackLayer)
  add(inners);
  setBorder(new LineBorder(ColorConstants.gray, 5))

}