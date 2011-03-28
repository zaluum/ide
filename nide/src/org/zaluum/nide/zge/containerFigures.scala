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
trait Container extends IFigure {
  def viewer : Viewer
  def viewerResources: ViewerResources
  def layer: Figure
  val helpers: Buffer[ShowHide]
  def feedbackLayer: Figure
  protected def itemAtIn(container:Figure,p:Point,debug:Boolean=false) : Option[Item]= container.findDeepAt(point(p), 0, debug) {
    case i: Item ⇒ i
  } 
  def itemAt(p: Point, debug: Boolean = false) :Option[Item]= itemAtIn(layer,p,debug) 
  def clear() {
    layer.removeAll()
    feedbackLayer.removeAll()
  }
}

trait BoxDefContainer extends Container {
  def boxDef: BoxDef
  def symbol: BoxTypeSymbol = boxDef.symbol.asInstanceOf[BoxTypeSymbol]
  def connectionsLayer: Figure
  def portsLayer: Figure
  override def itemAt(p:Point, debug:Boolean = false) = {
    super.itemAt(p, debug) 
    .orElse (itemAtIn(connectionsLayer,p,debug)) 
    .orElse (itemAtIn(portsLayer,p,debug)) 
  }
  private def portFigures = portsLayer.getChildren.collect { case p: PortFigure ⇒ p }
  def findPortFigure(boxName: Name, portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒
      p.valSym match {
        case Some(valSym) ⇒ (valSym.name == boxName && p.sym.name == portName && p.in == in)
        case None ⇒ false
      }
    }
  }
  def findPortFigure(portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒ p.valSym.isEmpty && p.sym.name == portName && p.in == in }
  }
  override def clear() {
    super.clear()
    connectionsLayer.removeAll()
    portsLayer.removeAll()
  }
  def owner: Symbol
  
  protected def createGraph  : ConnectionGraph= {
    val portVertexs = portsLayer.getChildren collect { case port: PortFigure ⇒ new PortVertex(port,port.anchor) }
    println ("ports " + portVertexs)
    val junctions = boxDef.junctions.collect { case j: Junction ⇒ (j -> new Joint(j.p)) }.toMap
    val edges = boxDef.connections.map {
      case c: ConnectionDef ⇒
        def toVertex(t: Tree, start: Boolean): Vertex = t match {
          case JunctionRef(name) ⇒ junctions.collect { case (k, joint) if (k.name == name) ⇒ joint }.head
          case p: PortRef ⇒ portVertexs.find { _.portPath == PortPath(p) }.getOrElse { throw new RuntimeException("could not find vertex for " + PortPath(p))}
        }
        (c -> new Edge(toVertex(c.a,true), toVertex(c.b,true), c.points,Some(c)).fixEnds)
    }.toMap
    println("graph edges = " + edges)
    new ConnectionGraphV(portVertexs.toSet ++ junctions.values, edges.values.toSet)
  }
  private var _currentBox:BoxDef = null
  private var _graph:ConnectionGraph = null
  def graph : ConnectionGraph = {
    if (_currentBox!=boxDef) {
      _graph = createGraph
      _currentBox = boxDef
    }
    _graph
  }
  def createFigures() {
    boxDef.children foreach {
      _ match {
        case EmptyTree ⇒
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) ⇒
          v.scope.lookupBoxTypeLocal(typeName) match {
            case Some(tpe) ⇒
              helpers += new OpenBoxFigure(v,
                tpe.decl.asInstanceOf[BoxDef],
                v.symbol.owner,
                BoxDefContainer.this,
                viewer,
                viewerResources)
            case None ⇒
              v.params.headOption match {
                case Some(p:Param) => 
                  helpers += new DirectValFigure(v, p, BoxDefContainer.this)
                case _ =>
                  helpers += new ImageValFigure(v, BoxDefContainer.this)
              }
          }
        case j@Junction(name,pos) =>
          helpers += new PointFigure(pos,BoxDefContainer.this,ColorConstants.green)
        case _ ⇒
      }
    }
  }
  def createConnectionFigures : Set[Item] = {
    graph.edges map { e => new ConnectionFigure(e, BoxDefContainer.this) }
  }
}

class OpenBoxFigure(
  val valTree: ValDef,
  val boxDef: BoxDef,
  val owner: Symbol,
  val container: BoxDefContainer,
  val viewer: Viewer,
  val viewerResources: ViewerResources) extends SimpleItem with TreeItem with ResizableFeedback with BoxDefContainer with Transparent {
  // Item
  type T = ValDef
  def tree = valTree
  def myLayer = container.layer
  def pos = tree.pos
  def size = valTree.size getOrElse Dimension(100, 100)
  // layers
  val inners = new LayeredPane
  val layer = new Layer
  val portsLayer = new Layer
  val connectionsLayer = new Layer
  val feedbackLayer = new Layer
  // BoxDefContainer
  override def useLocalCoordinates = true
  override def newConnectionFigures : Set[Item] = { super.newConnectionFigures}
  def populateFigures() {
    boxDef.children foreach {
      _ match {
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
          def newFig(left: Boolean) = helpers += new OpenPortDeclFigure(p, left, OpenBoxFigure.this)
          in match {
            case In ⇒ newFig(true)
            case Out ⇒ newFig(false)
            case Shift ⇒ newFig(true); newFig(false)
          }
        case _ ⇒
      }
    }
    boxDef.symbol match {
      case s: BoxTypeSymbol ⇒
        for (sup ← s.superSymbol; p ← sup.ports.values) {
          p match {
            case p: PortSymbol ⇒
              helpers += new PortSymbolFigure(p, OpenBoxFigure.this)
          }
        }
    }
  }
  override def paintClientArea(graphics: Graphics) {
    val rect = new Rectangle
    if (useLocalCoordinates()) {
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
  inners.add(layer)
  inners.add(portsLayer)
  inners.add(connectionsLayer)
  inners.add(feedbackLayer)
  add(inners);
  setBorder(new LineBorder(ColorConstants.gray, 5))

}