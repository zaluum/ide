package org.zaluum.nide.zge

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Buffer
import org.eclipse.draw2d.geometry.{ Dimension ⇒ EDimension }
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint }
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.Viewport
import org.zaluum.nide.utils.Utils.asRunnable
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.{ Point ⇒ MPoint }
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Vector2
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.geometry.Translatable
import org.zaluum.nide.compiler.Rect
import org.eclipse.draw2d.geometry.Point

object FigureHelper {
  import scala.collection.JavaConversions._
  def isOrHas(item: IFigure, lookingFor: IFigure): Boolean = {
    if (item eq lookingFor) true
    else {
      val children = item.getChildren.asInstanceOf[java.util.List[IFigure]]
      children.exists { isOrHas(_, lookingFor) }
    }
  }
  import org.zaluum.nide.zge._
  def nearest[T <: Figure](l: Seq[T], p: Point): Option[T] = {
    if (l.size > 0)
      Some(l.min(Ordering.fromLessThan[T]((a, b) ⇒ richFigure(a).distance(p) < richFigure(b).distance(p))))
    else None
  }
}
class RichFigure(fig: IFigure) {
  import scala.collection.JavaConversions._
  def children = fig.getChildren.asInstanceOf[java.util.List[IFigure]].toStream
  def deepChildren: Stream[IFigure] = {
    val deepChildren: Stream[IFigure] = children.flatMap { _.deepChildren }
    children ++ deepChildren
  }
  def safeRemove(i: IFigure): Unit = {
    if (fig.getChildren.contains(i))
      fig.remove(i)
  }
  def translateToViewport(p: MPoint): MPoint = rpoint(translateToViewport_!(point(p)))
  // translates a point in figs coordinates to viewport
  def translateToViewport_![A <: Translatable](p: A): A = {
    if (fig.isInstanceOf[Viewport]) p
    else {
      fig.translateToParent(p)
      fig.getParent.translateToViewport_!(p)
    }
  }

  def translateFromViewport(p: MPoint): MPoint = {
    val ep = point(p)
    rpoint(translateFromViewport_!(ep))
  }
  def translateFromViewport_![A <: Translatable](p: A): A = {
    if (fig.isInstanceOf[Viewport]) p
    else {
      // FIXME hard to reproduce bug involving the scrollbar? or singlecontainer?
      val ep = fig.getParent.translateFromViewport_!(p)
      fig.translateFromParent(ep)
      ep
    }
  }
  def distance(p: Point) = {
    val b = fig.getBounds
    if (b.contains(p)) 0
    else {
      List(
        java.awt.geom.Line2D.ptSegDist(b.left, b.top, b.right, b.top, p.x, p.y),
        java.awt.geom.Line2D.ptSegDist(b.left, b.bottom, b.right, b.bottom, p.x, p.y),
        java.awt.geom.Line2D.ptSegDist(b.left, b.top, b.left, b.bottom, p.x, p.y),
        java.awt.geom.Line2D.ptSegDist(b.right, b.top, b.right, b.bottom, p.x, p.y))
        .min
    }
  }
  def deepChildrenNear(abs: EPoint, radius: Double): List[(IFigure, Double)] = {
      def distance(f: IFigure): Double = f.getBounds.getCenter.getDistance(f.translateFromViewport_!(abs.getCopy))
    val v = for (c ← deepChildren; val d = distance(c); if (d < radius)) yield (c, d)
    v.toList
  }
  def findDeepAt[A](internalCoords: EPoint, deep: Int = 0, debug: Boolean = false)(partial: PartialFunction[IFigure, A]): Option[A] = {
    // bounds in parent coordinates
    // client area in relative coordinates 
    val spaces = new String(Array.fill(deep)(' '))
      def println2(str: String) = { if (debug) println(spaces + str) }
    var candidate: Option[A] = None
    val parentCoords = internalCoords.getCopy
    fig.translateToParent(parentCoords)
    println2("findDeep " + fig + " " + parentCoords + "bounds " + fig.getBounds + " visible=" + fig.isVisible + "opaque=" + fig.isOpaque)
    if (fig.isVisible && fig.containsPoint(parentCoords)) {
      candidate = partial.lift(fig)
      println2("candidate = " + candidate)
      println2("contains point. Client area= " + fig.getClientArea + " relative point=" + internalCoords)
      if (fig.getClientArea.contains(internalCoords)) {
        println2("checking children")
        // search children 
        val list = fig.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
        for (c ← list.reverse) {
          val childCoords = internalCoords.getCopy
          c.translateFromParent(childCoords)
          println2("checking child " + c + " with coordinates " + childCoords)
          c.findDeepAt(childCoords, deep + 1, debug)(partial) match {
            case Some(cc) ⇒
              println2("child found=" + cc)
              return Some(cc)
            case None ⇒
              println2("no child found")
          }
        }
      }
    }
    println2("returning candidate=" + candidate)
    return candidate
  }

  def findDeepContainerAt[A](internalCoords: EPoint)(partial: PartialFunction[IFigure, A]): Option[A] = {
    var candidate: Option[A] = None
    if (fig.isVisible && fig.getClientArea.contains(internalCoords)) {
      candidate = partial.lift(fig)
      // search children 
      val list = fig.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
      for (c ← list.reverse) {
        val childInternalCoords = internalCoords.getCopy
        c.translateFromParent(childInternalCoords)
        c.findDeepContainerAt(childInternalCoords)(partial) match {
          case Some(cc) ⇒ return Some(cc)
          case None     ⇒
        }
      }
    }
    return candidate
  }
}
trait Transparent extends IFigure {
  import scala.collection.JavaConversions._
  override def containsPoint(x: Int, y: Int) = {
    val pt = new EPoint(x, y)
    translateFromParent(pt);
    if (getClientArea.contains(pt)) {
      getChildren.asInstanceOf[java.util.List[IFigure]].reverse exists (_.containsPoint(pt.x, pt.y))
    } else {
      getBounds.contains(x, y)
    }
  }
}
trait Hover extends Figure {
  def hover_=(b: Boolean)
  def hover
}
trait Item extends Hover {
  def myLayer: Figure
  def container: ContainerItem
  def pos: MPoint
  def size: Dimension
  def rect: Rect = Rect(pos.x, pos.y, size.w, size.h)
  val feed: ItemFeedbackFigure
  var _hover = false
  def viewer: ItemViewer = container.viewer
  init()
  protected def init() {
    myLayer.add(this)
  }

  def hover = _hover
  def hover_=(b: Boolean) {
    _hover = b
    if (b) showFeedback else hideFeedback
  }
  def baseSpace = 6
  def parentContainers: List[ContainerItem] = if (container == this) Nil else
    container :: container.parentContainers
  def isOverlapped = {
    val parents = parentContainers
    val absBounds = container.translateMineToViewport_!(getBounds.getCopy())

    val before = container.deepChildren.takeWhile(_ != this)
    val myChildren = this.deepChildren
    before exists {
      _ match {
        case c: OpenBoxFigure ⇒
          if (!parents.contains(c) && c != Item.this && !myChildren.contains(c)) {
            val boundsChild = c.getClientArea().getCopy()
            c.translateToViewport_!(boundsChild)
            absBounds.intersects(boundsChild)
          } else false
        case _ ⇒ false
      }
    }
  }
  def destroy() {
    if (this.getParent() == myLayer)
      myLayer.remove(this) // this is a bottleneck. Lineal remove + layout, specially when invoked from containeritem
    hideFeedback()
  }
  def showFeedback() {
    feed.show()
  }
  def hideFeedback() {
    feed.hide()
  }
  def moveFeed(loc: MPoint) {
    feed.setInnerLocation(point(loc))
  }
  def blink(on: Boolean)
  def blink() {
    blink(true);
    import org.zaluum.nide.utils.Utils._
    val display = container.viewer.display
    display.timerExec(100, blink(false))
    display.timerExec(200, blink(true))
    display.timerExec(300, blink(false))
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {
    feed.setInnerBounds(handle.deltaAdd(delta, getBounds))
  }
  protected def updateSize() {
    val rect = new Rectangle(pos.x, pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  def selectionSubject: Option[SelectionSubject] = None
}
/**
 * Paints a shadow if a a container overtakes
 */
trait OverlappedEffect extends Item {
  override def paintBorder(g: Graphics) {
    super.paintBorder(g)
    if (isOverlapped) {
      g.setBackgroundColor(ColorConstants.black)
      val w = 5
      g.fillRectangle(new Rectangle(getBounds.x + w, getBounds.bottom() - w, getBounds().width() - w, w))
      g.fillRectangle(new Rectangle(getBounds.right - w, getBounds.y + w, w, getBounds.height - 2 * w))

    }

  }
}
trait HasPorts extends Item {
  val ports = Buffer[PortFigure]()
  override def destroy() {
    super.destroy()
    for (p ← ports) {
      p.destroy()
    }
    ports.clear()
  }
}
trait OverlappedItem extends Item {
  def openBox: OpenBoxFigure
  def container = openBox.container
  def containerDisplacement = Vector2(openBox.pos.x, openBox.pos.y)
  def constantDisplacement: Vector2
  def extPos: MPoint
  def relPos = extPos + constantDisplacement
  def pos = extPos + constantDisplacement + containerDisplacement // abs coordinates
}
trait RectFeedback {
  self: Item ⇒
  val feed = new ItemFeedbackFigure(container)
}

trait ResizableFeedback extends RectFeedback {
  self: Item ⇒
  override val feed: ResizeItemFeedbackFigure = new ResizeItemFeedbackFigure(this, container)
}
