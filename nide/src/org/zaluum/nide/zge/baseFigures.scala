package org.zaluum.nide.zge

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
import org.zaluum.nide.model.{ Point ⇒ MPoint, Dimension, Vector2, Resizable, Line, Positionable, Route }
import org.zaluum.nide.newcompiler.{ Tree, PortSymbol, PortDef, ConnectionDef, ValSymbol, ValDef, BoxTypeSymbol, NoSymbol, PortRef, ValRef, EmptyTree, ThisRef, In, PortDir, Out, Shift, BoxDef, Traverser, Symbol, Name }
import scala.collection.mutable.Buffer

object draw2dConversions {
  implicit def point(p: MPoint): EPoint = new EPoint(p.x, p.y)
  implicit def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
  implicit def rpoint(p: EPoint): MPoint = MPoint(p.x, p.y)
  implicit def rdimension(d: EDimension): Dimension = Dimension(d.width, d.height)
}
object FiguresHelper {
  import scala.collection.JavaConversions._
  def findDeepAt[A](container: IFigure, myCoordinates: EPoint)(partial: PartialFunction[IFigure, A]): Option[A] = {
    var candidate: Option[A] = None
    val rel = myCoordinates.getCopy
    container.translateFromParent(myCoordinates)
    if (container.isVisible && container.getClientArea.contains(rel)) {
      candidate = partial.lift(container)
      // search children 
      val list = container.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
      for (c ← list) {
        val childCoord = myCoordinates.getCopy
        c.translateFromParent(childCoord)
        findDeepAt(c, childCoord)(partial) match {
          case Some(cc) ⇒ return Some(cc)
          case None ⇒
        }
      }
    }
    return candidate
  }
}

trait Selectable extends Figure with Feedback
trait Feedback {
  def showFeedback()
  def hideFeedback()
}
trait Item extends Selectable {
  type T <: Tree
  def tree: T
  def container: BoxDefContainer
  def myLayer: Figure
  def pos: MPoint
  def size: Dimension
  val feed: ItemFeedbackFigure
  def showFeedback() {
    container.feedbackLayer.add(feed)
    update()
  }
  def hideFeedback() {
    if (container.feedbackLayer.getChildren.contains(feed))
      container.feedbackLayer.remove(feed)
  }
  def update() {
    items.clear
    val rect = new Rectangle(pos.x, pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  val items = Buffer[Item]()
  def show() {
    update()
    items.foreach { _.show() }
    myLayer.add(this)
  }
  def hide() {
    myLayer.remove(this)
    items.foreach { _.hide() }
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
