package org.zaluum.nide.zge

import org.eclipse.draw2d.Ellipse
import org.eclipse.swt.SWT
import org.eclipse.draw2d.XYLayout
import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.Shape
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.Cursors
import org.zaluum.nide.model._ 


object HandleSizes {
  val expansion = 8
}
import HandleSizes._
class FeedbackRectangle(val feed: ResizeFeedbackFigure) extends RectangleFigure {
  def boxFigure = feed.bf
}
class HandleRectangle(val x: Int, val y: Int, feed: ResizeFeedbackFigure) extends FeedbackRectangle(feed) {
  setBackgroundColor(ColorConstants.lightBlue)
  setForegroundColor(ColorConstants.white)
  def resizeCursor = {
    (x, y) match {
      case (0, 0) => Cursors.SIZENW
      case (1, 0) => Cursors.SIZEN
      case (2, 0) => Cursors.SIZENE
      case (0, 1) => Cursors.SIZEW
      case (1, 1) => Cursors.SIZEALL
      case (2, 1) => Cursors.SIZEE
      case (0, 2) => Cursors.SIZESW
      case (1, 2) => Cursors.SIZES
      case (2, 2) => Cursors.SIZESE
      case _ => Cursors.SIZEALL
    }
  }
  def deltaAdd(delta: (Int, Int), b: Rectangle) = {
    val (dx, dy) = delta
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
    val posx = inside.x + (inside.width / 2.0) * x - (expansion/2.0)
    val posy = inside.y + (inside.height / 2.0) * y - (expansion/2.0)
    setLocation(new Point(posx, posy))
  }
}
class PortFigure(val bf:BoxFigure,val p:Port, viewer:Viewer) extends Ellipse {
  setAntialias(1)
  def show(){
    viewer.portsLayer.add(this)
    update()
  }
  def hide(){
    viewer.portsLayer.remove(this)
  }
  def update(){
    val x = bf.getBounds.x + p.pos._1
    val y = bf.getBounds.y + p.pos._2
    setLocation(new Point(x,y))
    setSize(10,10)
  }
}
class ResizeFeedbackFigure(val bf: BoxFigure) extends Figure {
  
  val handles =
    (for {
      i <- 0 to 2;
      j <- 0 to 2;
      if (!(i == 1 && j == 1))
    } yield new HandleRectangle(i, j, this)).toList

  val rectangle = new FeedbackRectangle(this)
  rectangle.setLineStyle(SWT.LINE_DOT);
  add(rectangle)
  handles foreach (add(_))
  
  def innerLocation = rectangle.getLocation
  def innerBounds = rectangle.getBounds.getCopy
  
  def setInnerLocation(innerp: Point) {
    val bounds = getBounds.getCopy
    bounds.setLocation(innerp)
    bounds.setSize(rectangle.getSize)
    setInnerBounds(bounds)
  }
  
  def setInnerBounds(inside: Rectangle) {
    val outside = new Rectangle(inside)
    outside.expand(expansion, expansion)
    setBounds(outside)
    rectangle.setBounds(inside)
    rectangle.setFill(false)
    handles foreach (_.doPosition(outside, inside))
  }
}
