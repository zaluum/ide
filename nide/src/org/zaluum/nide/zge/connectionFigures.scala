package org.zaluum.nide.zge

import scala.collection.mutable.Buffer

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Ellipse
import org.eclipse.draw2d.Graphics
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.ConnectionDef
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Type
import org.zaluum.nide.compiler.Vector2

import draw2dConversions._

case class LineSelectionSubject(c: ConnectionDef, l: Line) extends SelectionSubject
class LineItem(val container: ContainerItem) extends Item with RectFeedback {
  var con: Option[ConnectionFigure] = None
  var complete = true
  var l: Line = _
  def helpers = List()
  def update(con: Option[ConnectionFigure], complete: Boolean, bad:Boolean, l: Line) {
    this.con = con
    this.complete = complete
    this.l = l
    if (bad)
      setForegroundColor(ColorConstants.red)
    else
      setForegroundColor(Colorizer.color(connectionDef.map { _.tpe }.getOrElse { null }))
    width = 1
    if (complete) {
      style = SWT.LINE_SOLID
    } else {
      style = SWT.LINE_DOT
    }
    if (bad) {
      width = 3
      style = SWT.LINE_DASH
    }

  }
  val tolerance = 4
  def expand = ((width + 2) / 2.0f).asInstanceOf[Int]
  override def selectionSubject = for (cf ← con; cdef ← cf.e.srcCon) yield LineSelectionSubject(cdef, l)
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
  def connectionDef = for (cf ← con; cdef ← cf.e.srcCon) yield cdef
  def blink(c: Boolean) {}
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
  def myLayer = if (con.isDefined) container.connectionsLayer else container.feedbackLayer
}

class PointFigure extends Ellipse {
  def update(p: Point, tpe: Type) = {
    setSize(5, 5)
    setFill(true)
    setLocation(point(p + Vector2(-2, -2)))
    val color = Colorizer.color(tpe)
    setBackgroundColor(color)
    setForegroundColor(color)
  }
}
class ConnectionPainter(container: ContainerItem) {
  val lines = Buffer[LineItem]()
  def paintCreatingRoute(edge: Edge) {
    paintRoute(edge, false, false, false)
  }
  def paintRoute(edge: Edge, feedback: Boolean, complete: Boolean, bad:Boolean, con: Option[ConnectionFigure] = None) {
    clear()
    edge.lines foreach { l ⇒
      val nl = new LineItem(container)
      nl.update(con, complete, bad, l)
      lines += nl
    }
    lines foreach { l ⇒ l.show }
    if (feedback) lines foreach { _.showFeedback() }
  }
  def clear() {
    lines.foreach { _.hide() }
    lines.clear
  }
}
// TODO not really a figure right now... no children
class ConnectionFigure(val e: Edge, val container: ContainerItem) extends Item {
  val painter = new ConnectionPainter(container)
  var feedback = false
  def size = null
  def pos = null
  val feed = null
  def myLayer = null
  def paint = painter.paintRoute(e, feedback, e.isComplete, e.isBad, Some(this))
  def blink(c: Boolean) = {}
  override def show() = {
    container.connectionsLayer.add(this);
    paint
  }
  override def hide() {
    if (container.connectionsLayer.getChildren.contains(this))
      container.connectionsLayer.remove(this)
    painter.clear()
  }
  override def showFeedback() {
    feedback = true
    paint
  }
  override def hideFeedback() {
    feedback = false
    paint
  }
  override def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {}
  override def moveFeed(p: Point) {}
  override def selectionSubject = e.srcCon
}
