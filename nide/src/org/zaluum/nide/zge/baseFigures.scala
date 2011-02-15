package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{ Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.zaluum.nide.model.{ Point ⇒ MPoint, Dimension, Vector2 }
import org.zaluum.nide.newcompiler.Tree
import scala.collection.mutable.Buffer

object draw2dConversions {
  implicit def point(p: MPoint): EPoint = new EPoint(p.x, p.y)
  implicit def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
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
import RichFigure._
class RichFigure(container: IFigure) {
  import scala.collection.JavaConversions._
  def deepChildren : List[IFigure] = {
    val immediateChildren = container.getChildren.asInstanceOf[java.util.List[IFigure]].toList
    val deepChildren:List[IFigure] = immediateChildren.flatMap { _.deepChildren }.toList
    immediateChildren ++ deepChildren 
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
      //if (container.isOpaque) 
      candidate = partial.lift(container)
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
            case Some(cc) ⇒ return Some(cc)
            case None ⇒
          }
        }
      }
    }
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

trait Selectable extends Figure with Feedback with ShowHide
trait Feedback {
  def showFeedback()
  def hideFeedback()
}
trait ShowHide {
  def show()
  def hide()
}
trait Item extends Selectable {
  type T <: Tree
  def tree :T
  def container :BoxDefContainer
  def moveFeed(loc: MPoint) 
  def moveDeltaFeed(delta: Vector2) 
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) 
}
trait SimpleItem extends Item {
  def myLayer: Figure
  def pos: MPoint
  def size: Dimension
  val feed: ItemFeedbackFigure
  val helpers = Buffer[ShowHide]()
  def showFeedback() {
    container.feedbackLayer.add(feed)
    update()
  }
  def hideFeedback() {
    if (container.feedbackLayer.getChildren.contains(feed))
      container.feedbackLayer.remove(feed)
  }
  def update() {
    helpers.clear
    val rect = new Rectangle(pos.x, pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  def show() {
    update()
    helpers.foreach { _.show() }
    myLayer.add(this)
  }
  def hide() {
    myLayer.remove(this)
    helpers.foreach { _.hide() }
  }
  def moveFeed(loc: MPoint) {
    feed.setInnerLocation(loc)
  }
  def moveDeltaFeed(delta: Vector2) {
    val loc = pos + delta
    feed.setInnerLocation(loc)
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {
    feed.setInnerBounds(handle.deltaAdd(delta, getBounds))
  }
}
trait RectFeedback extends Item {
  val feed = new ItemFeedbackFigure(container)
}

trait ResizableFeedback extends RectFeedback {
  override val feed: ResizeItemFeedbackFigure = new ResizeItemFeedbackFigure(this, container)
}
