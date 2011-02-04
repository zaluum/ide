package org.zaluum.nide.zge

import org.zaluum.nide.model.Route
import org.zaluum.nide.newcompiler.EmptyTree
import org.zaluum.nide.newcompiler.BoxRef
import org.zaluum.nide.newcompiler.PortRef
import org.zaluum.nide.newcompiler.NoSymbol
import org.zaluum.nide.newcompiler.BoxTypeSymbol
import org.zaluum.nide.newcompiler.ValDef
import org.zaluum.nide.model.Positionable
import org.zaluum.nide.newcompiler.ValSymbol
import org.zaluum.nide.newcompiler.ConnectionDef
import org.zaluum.nide.model.Line
import org.zaluum.nide.newcompiler.PortDef
import org.zaluum.nide.newcompiler.PortSymbol
import org.zaluum.nide.model.Resizable
import org.zaluum.nide.model.Vector2
import org.zaluum.nide.model.Dimension
import org.zaluum.nide.model.Point
import org.zaluum.nide.newcompiler.{ Tree }
import draw2dConversions._
import org.eclipse.draw2d.{ FreeformLayer, Ellipse, ColorConstants, Figure, ImageFigure, Polyline }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image

object draw2dConversions {
  implicit def point(p: Point): EPoint = new EPoint(p.x, p.y)
  implicit def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
  implicit def rpoint(p: EPoint): Point = Point(p.x, p.y)
  implicit def rdimension(d: EDimension): Dimension = Dimension(d.width, d.height)
}
trait CanShowFeedback extends Figure {
  def showFeedback()
  def hideFeedback()
}
trait CanShowUpdate extends Figure {
  def update()
}
trait Selectable extends Figure with CanShowFeedback
trait ItemFigure extends Selectable with CanShowUpdate {
  def viewer: AbstractViewer
  def positionable: Positionable
  def feed: ItemFeedbackFigure
  def size: Dimension
  def tree: Tree
  def showFeedback() {
    viewer.feedbackLayer.add(feed)
    update()
  }
  def hideFeedback {
    if (viewer.feedbackLayer.getChildren.contains(feed))
      viewer.feedbackLayer.remove(feed)
  }
  def update() {
    val rect = new Rectangle(positionable.pos.x, positionable.pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  def moveFeed(loc: Point) {
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
    viewer.layer.add(this)
  }
  show()
}

trait ResizableItemFigure extends ItemFigure {
  def feed: ResizeItemFeedbackFigure
  def resizable: Resizable
}

trait BoxFigure extends ItemFigure {
  def tree: ValDef
  def sym = tree.symbol match {
    case v: ValSymbol ⇒ Some(v)
    case NoSymbol ⇒ None
  }
  def treeView: TreeView
  def positionable = tree
  lazy val feed = new ItemFeedbackFigure(treeView.viewer)
  override def update() {
    super.update()
    sym.map {
      _.tpe match {
        case b: BoxTypeSymbol ⇒
          b.ports.values foreach {
            case s: PortSymbol ⇒
              new PortFigure(s.extPos + Vector2(getBounds.x, getBounds.y), s, sym, treeView)
          }
        case _ ⇒
      }
    }
  }
}
object PortDeclFigure {
  def img(in: Boolean) = "org/zaluum/nide/icons/portDecl" + (if (in) "In" else "Out") + ".png"
}

class PortDeclFigure(val tree: PortDef, val treeView: TreeView) extends ImageFigure with ItemFigure {
  def sym = tree.symbol match {
    case NoSymbol ⇒ None
    case p: PortSymbol ⇒ Some(p)
  }
  def positionable = tree
  override def viewer = treeView.viewer
  var size = Dimension(50, 20)
  lazy val feed = new ItemFeedbackFigure(treeView.viewer)
  def position = tree.inPos  + (if (tree.in) Vector2(48, 8) else Vector2(0, 8))

  override def update() {
    val image = viewer.imageFactory.get(PortDeclFigure.img(tree.in)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    sym foreach { new PortFigure(position, _, None, treeView) }
    super.update()
  }
}

class PortFigure(val pos: Point, val sym: PortSymbol, val valSym: Option[ValSymbol], treeView: AbstractModelView) extends Ellipse with CanShowFeedback with CanShowUpdate {
  setAntialias(1)
  setAlpha(50)
  setOutline(false)
  val highlight = ColorConstants.blue
  val normal = ColorConstants.gray
  setBackgroundColor(normal)
  def viewer = treeView.viewer
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
    setLocation(Point(x, y))
  }
  def show() {
    update()
    viewer.portsLayer.add(this)
  }
  show()
}

class ImageBoxFigure(val tree: ValDef, val treeView: TreeView) extends ImageFigure with BoxFigure {
  override def viewer = treeView.viewer
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)

  override def update() {
    setImage(viewer.imageFactory(tree))
    super.update()
  }
}

class LineFigure(l: Line, val cf: ConnectionFigure, treeView: TreeView) extends Polyline with CanShowUpdate with Selectable {
  //setAntialias(1)
  def viewer = treeView.viewer
  setForegroundColor(ColorConstants.gray)
  var complete = false
  var feedback = false
  setStart(new Point(l.from.x, l.from.y))
  setEnd(new Point(l.end.x, l.end.y))
  viewer.connectionsLayer.add(this)
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
  def update() {}
}
// TODO not really a figure right now... no children
class ConnectionFigure(val tree: ConnectionDef, treeView: TreeView) extends Figure with CanShowUpdate {
  {
    def portFigure(tree: Tree): Option[PortFigure] = tree match {
      case PortRef(portName, BoxRef(boxName)) ⇒ treeView.findPortFigure(boxName, portName)
      case PortRef(portName, EmptyTree) ⇒ treeView.findPortFigure(portName)
      case _ ⇒ None
    }
    def position(tree:Tree) : Option[Point] = portFigure(tree) map { p => p.anchor}
    val route = (position(tree.a), position(tree.b)) match {
      case (Some(a), Some(b)) ⇒ Some(Route(a, b))
      case _ ⇒ None
    }
    println("connection from " +  tree.a + " " + portFigure(tree.a))
    println("connection to " + tree.b + " " + portFigure(tree.b))
    route foreach { _.lines foreach { l ⇒ new LineFigure(l, this, treeView) } }
  }
  def update() {}
}