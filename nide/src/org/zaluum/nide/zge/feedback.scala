package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.geometry.PrecisionPoint
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Cursors
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.Vector2
import HandleSizes.expansion
import org.eclipse.draw2d.geometry.Dimension

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
      case _      ⇒ Cursors.SIZEALL
    }
  }
  def coords(r: Rectangle) = {
    (x, y) match {
      case (0, 0) ⇒ r.getTopLeft()
      case (1, 0) ⇒ r.getTop()
      case (2, 0) ⇒ r.getTopRight()
      case (0, 1) ⇒ r.getLeft()
      case (1, 1) ⇒ r.getCenter()
      case (2, 1) ⇒ r.getRight()
      case (0, 2) ⇒ r.getBottomLeft()
      case (1, 2) ⇒ r.getBottom()
      case (2, 2) ⇒ r.getBottomRight()
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
  def doPosition(inSize: Dimension) {
    setSize(expansion, expansion)
    val posx = expansion / 2.0 + (inSize.width / 2.0) * x
    val posy = expansion / 2.0 + (inSize.height / 2.0) * y
    setLocation(new PrecisionPoint(posx, posy))
  }
}
trait FeedbackFigureAbs extends Figure {
  def container: ContainerItem
  def viewer = container.viewer
  def toAbs(rect: Rectangle) = {
    container.translateMineToViewport_!(rect.getCopy())
  }
  override def setBounds(bounds: Rectangle) {
    super.setBounds(toAbs(bounds))
  }
  def show() {
    if (this.getParent() == null)
      viewer.feedbackLayer.add(this)
  }
  def hide() {
    if (this.getParent == viewer.feedbackLayer)
      viewer.feedbackLayer.remove(this)
  }
}
class ItemFeedbackFigure(val container: ContainerItem) extends FeedbackFigureAbs {
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
    rectangle.setBounds(getBounds.getCopy())
  }

}
class ResizeItemFeedbackFigure(val bf: Item, parent: ContainerItem) extends ItemFeedbackFigure(parent) {
  override def useLocalCoordinates() = true
  val handles =
    (for {
      i ← 0 to 2;
      j ← 0 to 2;
      if (!(i == 1 && j == 1))
    } yield new HandleRectangle(i, j, this)).toList
  rectangle.setLocation(new Point(expansion, expansion));
  rectangle.setFill(false)
  handles foreach (add(_))
  override def setInnerBounds(inside: Rectangle) {
    innerBounds = inside
    val outside = new Rectangle(inside)
    outside.expand(expansion, expansion)
    setBounds(outside)
    rectangle.setSize(inside.getSize())
    handles foreach (_.doPosition(inside.getSize()))
  }
}
