package org.zaluum.nide.zge

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.Buffer

import org.eclipse.draw2d.geometry.{ Dimension ⇒ EDimension }
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint }
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.Viewport
import org.zaluum.nide.Utils.asRunnable
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.{ Point ⇒ MPoint }
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Vector2
import draw2dConversions._

object draw2dConversions {
  def point(p: MPoint): EPoint = new EPoint(p.x, p.y)
  def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
  implicit def rpoint(p: EPoint): MPoint = MPoint(p.x, p.y)
  implicit def rdimension(d: EDimension): Dimension = Dimension(d.width, d.height)
}
object FigureHelper {
  import scala.collection.JavaConversions._
  def isOrHas(item: IFigure, lookingFor: IFigure): Boolean = {
    if (item eq lookingFor) true
    else {
      val children = item.getChildren.asInstanceOf[java.util.List[IFigure]]
      children.exists { isOrHas(_, lookingFor) }
    }
  }
}
object RichFigure {
  implicit def richFigure(f: IFigure) = new RichFigure(f)
}
class RichFigure(container: IFigure) {
  import RichFigure._
  import scala.collection.JavaConversions._
  def immediateChildren = container.getChildren.asInstanceOf[java.util.List[IFigure]].toList
  def deepChildren: List[IFigure] = {
    val deepChildren: List[IFigure] = immediateChildren.flatMap { _.deepChildren }.toList
    immediateChildren ++ deepChildren
  }
  def safeRemove(i: IFigure): Unit = {
    if (container.getChildren.contains(i))
      container.remove(i)
  }
  def translateToViewport(p: MPoint): MPoint = translateToViewport(point(p))
  def translateToViewport(p: EPoint): EPoint = {
    if (container.isInstanceOf[Viewport]) p.getCopy
    else {
      val ep = container.getParent.translateToViewport(p)
      container.translateToParent(ep)
      ep
    }
  }
  def translateFromViewport(p: MPoint): MPoint = translateFromViewport(point(p))
  def translateFromViewport(p: EPoint): EPoint = {
    if (container.isInstanceOf[Viewport]) p.getCopy
    else {
      // FIXME rare bug
      if (container.getParent == null) println("null parent " + container)
      val ep = container.getParent.translateFromViewport(p)
      container.translateFromParent(ep)
      ep
    }
  }
  def deepChildrenNear(abs: EPoint, radius: Double): List[(IFigure, Double)] = {
      def distance(f: IFigure): Double = f.getBounds.getCenter.getDistance(f.translateFromViewport(abs))
    val v = for (c ← deepChildren.view; val d = distance(c); if (d < radius)) yield (c, d)
    v.toList
  }
  def findDeepAt[A](internalCoords: EPoint, deep: Int = 0, debug: Boolean = false)(partial: PartialFunction[IFigure, A]): Option[A] = {
    // bounds in parent coordinates
    // client area in relative coordinates 
    val spaces = new String(Array.fill(deep)(' '))
      def println2(str: String) = { if (debug) println(spaces + str) }
    var candidate: Option[A] = None
    val parentCoords = internalCoords.getCopy
    container.translateToParent(parentCoords)
    println2("findDeep " + container + " " + parentCoords + "bounds " + container.getBounds + " visible=" + container.isVisible + "opaque=" + container.isOpaque)
    if (container.isVisible && container.containsPoint(parentCoords)) {
      candidate = partial.lift(container)
      println2("candidate = " + candidate)
      println2("contains point. Client area= " + container.getClientArea + " relative point=" + internalCoords)
      if (container.getClientArea.contains(internalCoords)) {
        println2("checking children")
        // search children 
        val list = container.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
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
    if (container.isVisible && container.getClientArea.contains(internalCoords)) {
      candidate = partial.lift(container)
      // search children 
      val list = container.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
      for (c ← list.reverse) {
        val childInternalCoords = internalCoords.getCopy
        c.translateFromParent(childInternalCoords)
        c.findDeepContainerAt(childInternalCoords)(partial) match {
          case Some(cc) ⇒ return Some(cc)
          case None ⇒
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
  val feed: ItemFeedbackFigure
  var showing = true
  var _hover = false
  def hover = _hover
  def hover_=(b: Boolean) {
    _hover = b
    if (b) showFeedback else hideFeedback
  }
  def baseSpace = 6
  def show() {
    showing = true
    myLayer.add(this)
  }
  def hide() {
    if (showing) {
      showing = false
      myLayer.remove(this) // this is a bottleneck. Lineal remove + layout, specially when invoked from containeritem
      hideFeedback()
    }
  }
  def showFeedback() {
    container.feedbackLayer.add(feed)
  }
  def hideFeedback() {
    if (container.feedbackLayer.getChildren.contains(feed))
      container.feedbackLayer.remove(feed)
  }
  def moveFeed(loc: MPoint) {
    feed.setInnerLocation(point(loc))
  }
  def blink(on: Boolean)
  def blink() {
    blink(true);
    import org.zaluum.nide.Utils._
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
trait HasPorts extends Item {
  val ports = Buffer[PortFigure]()
  override def show() {
    super.show
    for (p ← ports) p.container.portsLayer.add(p)
  }
  override def hide() {
    super.hide
    for (p ← ports) {
      if (p.container.portsLayer.getChildren.contains(p))
        p.container.portsLayer.remove(p)
    }
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
