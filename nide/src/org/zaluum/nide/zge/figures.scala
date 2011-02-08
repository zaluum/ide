package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{ FreeformLayer, Ellipse, ColorConstants, Figure, ImageFigure, Polyline, ScalableFreeformLayeredPane, IFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.model.{ Point ⇒ MPoint, Dimension, Vector2, Resizable, Line, Positionable, Route }
import org.zaluum.nide.newcompiler.{ Tree, PortSymbol, PortDef, ConnectionDef, ValSymbol, ValDef, BoxTypeSymbol, NoSymbol, PortRef, ValRef, EmptyTree, ThisRef, In, PortDir, Out, Shift, BoxDef, Traverser, Symbol, Name }
import scala.collection.mutable.Buffer

object draw2dConversions {
  implicit def point(p: MPoint): EPoint = new EPoint(p.x, p.y)
  implicit def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
  implicit def rpoint(p: EPoint): MPoint = MPoint(p.x, p.y)
  implicit def rdimension(d: EDimension): Dimension = Dimension(d.width, d.height)
}
trait CanShowFeedback {
  def showFeedback()
  def hideFeedback()
}
trait CanShowUpdate {
  def show()
  def hide()
  def update()
}
trait Selectable extends Figure with CanShowFeedback
trait ItemFigure extends Figure with Selectable with CanShowUpdate {
  def bdf: BoxDefLayers
  def positionable: Positionable
  def feed: ItemFeedbackFigure
  def size: Dimension
  def tree: Tree
  def showFeedback() {
    bdf.feedbackLayer.add(feed)
    update()
  }
  def hideFeedback {
    if (bdf.feedbackLayer.getChildren.contains(feed))
      bdf.feedbackLayer.remove(feed)
  }
  def update() {
    val rect = new Rectangle(positionable.pos.x, positionable.pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  def moveFeed(loc: MPoint) {
    feed.setInnerLocation(loc)
  }
  def moveDeltaFeed(delta: Vector2) {
    val loc = positionable.pos + delta
    feed.setInnerLocation(loc)
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {
    feed.setInnerBounds(handle.deltaAdd(delta, getBounds))
  }
  def show() {
    update()
    bdf.layer.add(this)
  }
  def hide() {
    bdf.layer.remove(this)
  }
}

trait ResizableItemFigure extends ItemFigure {
  def feed: ResizeItemFeedbackFigure
  def resizable: Resizable
}

trait BoxFigure extends ItemFigure {
  def tree: ValDef
  def sym = tree.symbol.asInstanceOf[ValSymbol]
  def bdf: BoxDefLayers
  def positionable = tree
  var ports = List[PortFigure]()
  lazy val feed = new ItemFeedbackFigure(bdf)
  override def show() {
    super.show()
    ports.foreach { _.show }
  }
  override def hide() {
    super.show()
    ports.foreach { _.hide }
  }
  override def update() {
    super.update()
    ports = sym.tpe match {
      case b: BoxTypeSymbol ⇒
        b.ports.values.collect {
          case s: PortSymbol ⇒
            new PortFigure(s.extPos + Vector2(getBounds.x, getBounds.y), s, s.dir == In, Some(sym), bdf)
        }.toList
      case _ ⇒ List()
    }
  }
}
object PortDeclFigure {
  private def str(dir: PortDir) = dir match {
    case In ⇒ "In"
    case Out ⇒ "Out"
    case Shift ⇒ "Shift"
  }
  def img(dir: PortDir) = "org/zaluum/nide/icons/portDecl" + str(dir) + ".png"
}

class PortDeclFigure(val tree: PortDef, val bdf: BoxDefLayers) extends ImageFigure with ItemFigure {
  def sym = tree.symbol match {
    case NoSymbol ⇒ None
    case p: PortSymbol ⇒ Some(p)
  }
  def positionable = tree
  var size = Dimension(50, 20)
  lazy val feed = new ItemFeedbackFigure(bdf)
  def position = tree.inPos + (if (tree == In) Vector2(48, 8) else Vector2(0, 8))
  val portFig = sym map { new PortFigure(position, _, tree.dir == In, None, bdf) }

  override def update() {
    val image = bdf.viewer.imageFactory.get(PortDeclFigure.img(tree.dir)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    super.update()
  }
  override def show() {
    super.show
    portFig.foreach(_.show)
  }
  override def hide() {
    super.hide
    portFig.foreach(_.hide)
  }
}

class PortFigure(val pos: MPoint, val sym: PortSymbol, val in: Boolean, val valSym: Option[ValSymbol], bdf: BoxDefLayers) extends Ellipse with CanShowFeedback with CanShowUpdate {
  setAntialias(1)
  setAlpha(50)
  setOutline(false)
  val highlight = ColorConstants.blue
  val normal = ColorConstants.gray
  setBackgroundColor(normal)
  def showFeedback() {
    setBackgroundColor(highlight)
  }
  def hideFeedback() {
    setBackgroundColor(normal)
  }
  def anchor = getBounds.getCenter
  def update() {
    setSize(10, 10)
    val x = pos.x - getBounds.width / 2
    val y = pos.y - getBounds.height / 2
    setLocation(MPoint(x, y))
  }
  def show() {
    update()
    bdf.portsLayer.add(this)
  }
  def hide() {
    bdf.portsLayer.remove(this)
  }
}

class ImageBoxFigure(val tree: ValDef, val bdf: BoxDefLayers) extends ImageFigure with BoxFigure {
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)

  override def update() {
    setImage(bdf.viewer.imageFactory(tree.tpe))
    super.update()
  }
}
trait Layers extends CanShowUpdate {
  def viewer: TreeViewer // TODO only for image factory... remove?
  def layer: Figure
  def feedbackLayer: Figure
  def connectionsLayer: Figure
  def findDeepAt(container: IFigure, p: EPoint) = {
    Option(container.findFigureAt(p.x, p.y)) filter (_ != container)
  }
  private def findShallowAt(container: IFigure, p: EPoint) = {
    import scala.collection.JavaConversions._
    container.getChildren.asInstanceOf[java.util.List[IFigure]] find { _.containsPoint(p) };
  }
  def figureAt(p: EPoint) = findShallowAt(layer, p) map { case (bf: ItemFigure) ⇒ bf }
  def feedbackAt(p: EPoint) = findDeepAt(feedbackLayer, p)
  def lineAt(p: EPoint) = findDeepAt(connectionsLayer, p) map { case l: LineFigure ⇒ l }
}
trait BoxDefLayers extends Layers {
  def portsLayer: Figure
  def connectionsLayer: Figure
  def owner: Symbol
  def boxDef: BoxDef
  import scala.collection.JavaConversions._
  private def portFigures = portsLayer.getChildren.collect { case p: PortFigure ⇒ p }
  def findPortFigure(boxName: Name, portName: Name, in: Boolean): Option[PortFigure] =
    portFigures find { p ⇒
      p.valSym match {
        case Some(valSym) ⇒ (valSym.name == boxName && p.sym.name == portName && p.in == in)
        case None ⇒ false
      }
    }
  def findPortFigure(portName: Name, in: Boolean): Option[PortFigure] =
    portFigures find { p ⇒ p.valSym.isEmpty && p.sym.name == portName && p.in == in }
  def clear()

  def update() {
    clear()
    new Traverser(owner) {
      override def traverse(tree: Tree) {
        super.traverse(tree)
        tree match {
          case EmptyTree ⇒
          case p@PortDef(name, typeName, in, inPos, extPos) ⇒
            new PortDeclFigure(p, BoxDefLayers.this).show()
          case v@ValDef(name, typeName, pos, guiSize) ⇒
            new ImageBoxFigure(v, BoxDefLayers.this).show()
          case _ ⇒
        }
      }
    }.traverse(boxDef)
    // create connections (need to find figures positions)
    new Traverser(owner) {
      override def traverse(tree: Tree) {
        super.traverse(tree)
        tree match {
          case c@ConnectionDef(a, b) ⇒
            new ConnectionFigure(c, BoxDefLayers.this).show()
          case _ ⇒
        }
      }
    }.traverse(boxDef)
  }
}

class OpenBoxFigure(val valTree: ValDef, val boxDef: BoxDef, val viewer: TreeViewer, val owner: Symbol, val parentFig: Figure) extends Figure with BoxDefLayers {
  val layer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val feedbackLayer = new FreeformLayer
  val inners = new ScalableFreeformLayeredPane
  inners.add(layer)
  inners.add(portsLayer)
  inners.add(connectionsLayer)
  inners.add(feedbackLayer)
  add(inners)
  def clear { inners.removeAll }
  def show = { update(); parentFig.add(this) }
  def hide = parentFig.remove(this)
}
class LineFigure(l: Line, bdf: BoxDefLayers, val con: Option[ConnectionDef] = None) extends Polyline with CanShowUpdate with Selectable {
  //setAntialias(1)
  setForegroundColor(ColorConstants.gray)
  var complete = false
  var feedback = false
  showComplete
  def showFeedback { feedback = true; calcStyle }
  def hideFeedback { feedback = false; calcStyle }
  def showComplete { complete = true; calcStyle }
  def showIncomplete { complete = false; calcStyle }
  def calcStyle {
    if (feedback) {
      setLineStyle(SWT.LINE_DASH)
      setLineWidth(2)
    } else {
      setLineWidth(1)
      if (complete) {
        setLineStyle(SWT.LINE_SOLID)
      } else {
        setLineStyle(SWT.LINE_DOT)
      }
    }
  }
  def show() {
    update()
    bdf.connectionsLayer.add(this)
  }
  def hide() { bdf.connectionsLayer.remove(this) }
  def update() {
    setStart(new EPoint(l.from.x, l.from.y))
    setEnd(new EPoint(l.end.x, l.end.y))
    calcStyle
  }
}
class ConnectionPainter(bdf: BoxDefLayers) {
  val lines = Buffer[LineFigure]()
  def paintRoute(route: Route, con: Option[ConnectionDef] = None) {
    clear()
    route.lines foreach { l ⇒ lines += new LineFigure(l, bdf, con) }
    lines foreach { _.show }
  }
  def clear() {
    lines.foreach { _.hide }
    lines.clear
  }
}
// TODO not really a figure right now... no children
class ConnectionFigure(val tree: ConnectionDef, bdf: BoxDefLayers) extends Figure with CanShowUpdate {
  val painter = new ConnectionPainter(bdf)
  def calcRoute = {
    // TODO paint incomplete connections gracefully
    def portFigure(tree: Tree): Option[PortFigure] = tree match {
      case PortRef(v@ValRef(_), portName, in) ⇒ bdf.findPortFigure(v.symbol.name, portName, in)
      case PortRef(ThisRef, portName, in) ⇒ bdf.findPortFigure(portName, in)
      case _ ⇒ None
    }
    def position(tree: Tree): Option[MPoint] = portFigure(tree) map { p ⇒ p.anchor }
    val route = (position(tree.a), position(tree.b)) match {
      case (Some(a), Some(b)) ⇒ Some(Route(a, b))
      case _ ⇒ None
    }
    route
  }
  def update() {
    calcRoute match {
      case Some(r) ⇒ painter.paintRoute(r, Some(tree))
      case None ⇒ painter.clear()
    }
  }
  def show() {
    update()
  }
  def hide() {
    painter.clear()
  }
}