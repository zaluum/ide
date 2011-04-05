package org.zaluum.nide.zge

import org.zaluum.nide.compiler.Vector2
import HandleSizes._
import org.eclipse.draw2d.{ Cursors, RectangleFigure, Figure, ColorConstants, Shape }
import org.eclipse.draw2d.geometry.{ Rectangle, Point }
import org.eclipse.swt.SWT

object HandleSizes {
  val expansion = 8
}
class FeedbackRectangle(val feed: ItemFeedbackFigure) extends RectangleFigure {
}
class ResizeFeedbackRectangle(val feed: ResizeItemFeedbackFigure) extends RectangleFigure {
  def resizeItemFigure = feed.bf
}
class HandleRectangle(val x: Int, val y: Int, feed: ResizeItemFeedbackFigure) extends ResizeFeedbackRectangle(feed) {
  setBackgroundColor(ColorConstants.lightBlue)
  setForegroundColor(ColorConstants.white)
  def resizeCursor = {
    (x, y) match {
      case (0, 0) ⇒ Cursors.SIZENW
      case (1, 0) ⇒ Cursors.SIZEN
      case (2, 0) ⇒ Cursors.SIZENE
      case (0, 1) ⇒ Cursors.SIZEW
      case (1, 1) ⇒ Cursors.SIZEALL
      case (2, 1) ⇒ Cursors.SIZEE
      case (0, 2) ⇒ Cursors.SIZESW
      case (1, 2) ⇒ Cursors.SIZES
      case (2, 2) ⇒ Cursors.SIZESE
      case _ ⇒ Cursors.SIZEALL
    }
  }
  def deltaAdd(delta: Vector2, b: Rectangle) = {
    val dx = delta.x
    val dy = delta.y
    val res = new Rectangle(b)
    if (x == 0) {
      res.x += dx
      res.width -= dx
    } else if (x == 2) {
      res.width += dx
    }
    if (y == 0) {
      res.y += dy
      res.height -= dy
    } else if (y == 2) {
      res.height += dy
    }
    res
  }
  def doPosition(outside: Rectangle, inside: Rectangle) {
    setSize(expansion, expansion)
    val posx = inside.x + (inside.width / 2.0) * x - (expansion / 2.0)
    val posy = inside.y + (inside.height / 2.0) * y - (expansion / 2.0)
    setLocation(new Point(posx, posy))
  }
}

class ItemFeedbackFigure(parent: ContainerItem) extends Figure {
  protected val rectangle = new FeedbackRectangle(this)
  rectangle.setLineStyle(SWT.LINE_DOT);
  rectangle.setFill(false);
  add(rectangle)
  def innerLocation = innerBounds.getLocation
  var innerBounds = new Rectangle()

  def setInnerLocation(innerp: Point) {
    val bounds = new Rectangle(innerBounds)
    bounds.setLocation(innerp)
    setInnerBounds(bounds)
  }

  def setInnerBounds(inside: Rectangle) {
    innerBounds = inside
    val rectBounds = new Rectangle(inside).expand(2, 2)
    setBounds(rectBounds)
    rectangle.setBounds(rectBounds)
  }
  def show() {
    parent.feedbackLayer.add(this)
  }
  def hide() {
    if (parent.feedbackLayer.getChildren.contains(this))
      parent.feedbackLayer.remove(this)
  }

}
class ResizeItemFeedbackFigure(val bf: Item, parent: ContainerItem) extends ItemFeedbackFigure(parent) {

  val handles =
    (for {
      i ← 0 to 2;
      j ← 0 to 2;
      if (!(i == 1 && j == 1))
    } yield new HandleRectangle(i, j, this)).toList

  handles foreach (add(_))
  override def setInnerBounds(inside: Rectangle) {
    innerBounds = inside
    val outside = new Rectangle(inside)
    outside.expand(expansion, expansion)
    setBounds(outside)
    rectangle.setBounds(inside)
    rectangle.setFill(false)
    handles foreach (_.doPosition(outside, inside))
  }
}
