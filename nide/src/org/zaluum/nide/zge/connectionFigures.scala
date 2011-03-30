package org.zaluum.nide.zge

import org.eclipse.draw2d.Shape
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.Ellipse
import org.eclipse.swt.events.FocusListener
import org.eclipse.jface.viewers.ICellEditorListener
import org.eclipse.swt.widgets.Text
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.draw2d.text.TextFlow
import org.eclipse.draw2d.text.FlowPage
import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{ ColorConstants, Figure, ImageFigure, Polyline, Graphics }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler._
import scala.collection.mutable.Buffer
 
case class LineSelectionSubject(c: ConnectionDef, l: Line) extends SelectionSubject
class LineFigure(
  val l: Line,
  val r: Edge,
  val container: BoxDefContainer,
  val complete:Boolean,
  val con: Option[ConnectionFigure] = None) extends SimpleItem with RectFeedback {
  val tolerance = 4
  def expand = ((width + 2) / 2.0f).asInstanceOf[Int]
  
  override def selectionSubject = for (cf <-con; cdef <- cf.e.srcCon) yield LineSelectionSubject(cdef,l)
  override def getBounds: Rectangle = {
    if (bounds == null) {
      val (expandx, expandy) = if (l.horizontal) (0, expand) else (expand, 0)
      bounds = new Rectangle(point(l.start), point(l.end)).expand(expandx, expandy)
    }
    bounds
  }
  override def showFeedback() {
    feed.setInnerBounds(getBounds)
    super.showFeedback
  }
  def size = getBounds.getSize //special size
  def pos = getBounds.getLocation
  override def containsPoint(x: Int, y: Int) = {
    val t = math.max(expand, tolerance).asInstanceOf[Int];
    val b = getBounds.getCopy
    b.expand(t, t)
    b.contains(x, y)
  }
  val tpe = for (cf <- con; cdef <- cf.e.srcCon) yield  cdef.tpe
  def populateFigures = {
    setForegroundColor(Colorizer.color( tpe.getOrElse{NoSymbol} ))
    width = 1
    if (complete) {
      style = SWT.LINE_SOLID
    } else {
      style = SWT.LINE_DOT
    }
  }
  override def paintFigure(g: Graphics) = {
    g.setForegroundColor(getForegroundColor);
    g.setLineStyle(style)
    g.setLineWidth(width)
    g.drawLine(point(l.start), point(l.end))
    val w = ((width / 2.0f) + 1).asInstanceOf[Int]
    val (sv, ev, upv, downv) = if (l.horizontal)
      (Vector2(expand + 1, 0), Vector2(-expand - 1, 0), Vector2(0, -w), Vector2(0, w))
    else
      (Vector2(0, expand + 1), Vector2(0, -expand - 1), Vector2(-w, 0), Vector2(w, 0))

    val s = l.low + sv
    val e = l.high + ev
    val ups = s + upv
    val upe = e + upv
    val downs = s + downv
    val downe = e + downv
    g.setForegroundColor(ColorConstants.white)
    g.setLineStyle(SWT.LINE_SOLID)
    g.setLineWidth(1)
    g.drawLine(point(ups), point(upe))
    g.drawLine(point(downs), point(downe))
  }
  var style = SWT.LINE_SOLID
  var width = 1
  override def repaint() {
    bounds = null
    super.repaint()
  }
  def myLayer = if(con.isDefined) container.connectionsLayer else container.feedbackLayer
}

class PointFigure(p: Point, val container: BoxDefContainer, tpe: Type) extends Ellipse with Item {
  def show() {
    setSize(5, 5)
    setFill(true)
    setLocation(point(p + Vector2(-2, -2)))
    val color = Colorizer.color(tpe)
    setBackgroundColor(color)
    setForegroundColor(color)
    container.pointsLayer.add(this)
  }
  def showFeedback() {}
  def hideFeedback() {}
  def hide() {
    if (container.pointsLayer.getChildren.contains(this))
      container.pointsLayer.remove(this)
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) = {}
  def moveDeltaFeed(delta: Vector2) {}
  def moveFeed(p: Point) {}
}
class ConnectionPainter(bdf: BoxDefContainer) {
  val lines = Buffer[LineFigure]()
  val points = Buffer[PointFigure]()
  def paintCreatingRoute(edge: Edge) {
    paintRoute(edge, false, false)
  }
  def paintRoute(edge: Edge, feedback: Boolean, complete:Boolean, con: Option[ConnectionFigure] = None) {
    clear()
    edge.lines foreach { l ⇒ lines += new LineFigure(l, edge, bdf, complete, con) }
    //edge.points foreach { p => points += new PointFigure(p,bdf,ColorConstants.white) }
    if (feedback) lines foreach { l ⇒ l.showFeedback() }
    lines foreach { l ⇒ l.show }
    points foreach { _.show }
  }
  def clear() {
    lines.foreach { _.hide }
    points.foreach { _.hide }
    lines.clear
    points.clear
  }
}
// TODO not really a figure right now... no children
class ConnectionFigure(val e: Edge, val container: BoxDefContainer) extends Item {
  type T = ConnectionDef
  val painter = new ConnectionPainter(container)
  var feedback = false
  def paint = painter.paintRoute(e, feedback, true, Some(this))
  def show() = {
    container.connectionsLayer.add(this);
    paint
  }
  def hide() {
    if (container.connectionsLayer.getChildren.contains(this))
      container.connectionsLayer.remove(this)
    painter.clear()
  }
  def showFeedback() {
    feedback = true
    paint
  }
  def hideFeedback() {
    feedback = false
    paint
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {}
  def moveDeltaFeed(delta: Vector2) {}
  def moveFeed(p: Point) {}
}
