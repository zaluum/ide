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
import org.zaluum.nide.compiler.JavaType
import org.zaluum.nide.compiler.Vector2
import draw2dConversions._
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.Figure

case class LineSelectionSubject(c: ConnectionDef, l: Line) extends SelectionSubject {
  def selectedTree = c
}

trait Blinker extends Item {
  private var blinking = false
  protected var blinkStatus = false
  def startBlink() {
    blink(true);
    blinking = true
    import org.zaluum.nide.utils.Utils._
    val display = container.viewer.display
      def scheduleNext(): Unit =
        if (blinking) {
          blink(!blinkStatus)
          display.timerExec(200, scheduleNext())
        } else blink(false)
    scheduleNext()
  }
  def stopBlink() {
    blinking = false
  }
}
class LineFigure extends Figure {
  private var l: Line = null
  var width = 5
  def tolerance = 4
  var complete = true
  private var style: Int = SWT.LINE_SOLID
  def expand = ((width + 2) / 2.0f).asInstanceOf[Int]
  def setLine(l: Line, complete: Boolean, bad: Boolean, color: Color) {
    erase()
    this.l = l
    this.complete = complete
    if (bad)
      setForegroundColor(ColorConstants.red)
    else
      setForegroundColor(color)
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
    repaint()
  }
  override def getBounds() = {
    if (bounds == null) {
      if (l != null) {
        val (expandx, expandy) = if (l.horizontal) (0, expand) else (expand, 0)
        bounds = new Rectangle(point(l.start), point(l.end)).expand(expandx, expandy)
      } else
        bounds = new Rectangle()
    }
    bounds

  }
  override def containsPoint(x: Int, y: Int) = {
    val t = math.max(expand, tolerance).asInstanceOf[Int];
    val b = getBounds.getCopy
    b.expand(t, t)
    b.contains(x, y)
  }
  override def repaint() {
    bounds = null
    super.repaint()
  }
  override def setBounds(r: Rectangle) { throw new IllegalStateException }
  override def paintFigure(g: Graphics) = {
    g.setForegroundColor(getForegroundColor);
    g.setLineStyle(style)
    g.setLineWidth(width)
    val start = point(l.start)
    val end = point(l.end)
    g.drawLine(start, end)
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
}

class LineItem(val container: ContainerItem, val l: Line, val e: Edge) extends LineFigure with Item with Blinker with RectFeedback {
  def helpers = List()
  setLine(l, e.isComplete, e.isBad, Colorizer.color(connectionDef.map { _.tpe }.getOrElse { null }))
  override def selectionSubject = connectionDef map (LineSelectionSubject(_, l))
  override def showFeedback() {
    feed.setInnerBounds(getBounds)
    super.showFeedback
  }
  def size = getBounds.getSize //special size
  def pos = getBounds.getLocation
  def connectionDef = for (cdef ← e.srcCon) yield cdef
  protected var blinkColorBefore: Color = null
  def blink(c: Boolean) {
    if (blinkColorBefore == null) blinkColorBefore = getForegroundColor
    if (c) {
      setForegroundColor(ColorConstants.white)
    } else {
      setForegroundColor(blinkColorBefore)
      blinkColorBefore = null
    }
    blinkStatus = c
  }
  def myLayer = container.connectionsLayer
}

class PointFigure extends Ellipse {
  def update(p: Point, tpe: Option[JavaType]) = {
    setSize(5, 5)
    setFill(true)
    setLocation(point(p + Vector2(-2, -2)))
    val color = Colorizer.color(tpe)
    setBackgroundColor(color)
    setForegroundColor(color)
  }
}
class PreviewConnectionPainter(container: ContainerItem) {
  val lines = Buffer[LineFigure]()
  def translateToViewport(l: Line): Line = {
    val from = container.translateMineToViewport_!(point(l.from))
    val to = container.translateMineToViewport_!(point(l.to))
    Line(from, to, l.horizontal)
  }
  def paintRoute(edge: Edge) {
    clear()
    for (l ← edge.lines) {
      val f = new LineFigure()
      lines += f
      f.setLine(translateToViewport(l), false, false, ColorConstants.black)
      container.viewer.feedbackLayer.add(f)
    }
  }
  def clear() {
    lines.foreach { container.viewer.feedbackLayer.remove }
    lines.clear
  }
}
class ConnectionPainter(container: ContainerItem) {
  val lines = Buffer[LineItem]()
  def paintRoute(edge: Edge, feedback: Boolean) {
    clear()
    edge.lines foreach { l ⇒
      val nl = new LineItem(container, l, edge)
      lines += nl
    }
    if (feedback) lines foreach { _.showFeedback() }
  }
  def clear() {
    lines.foreach { _.destroy() }
    lines.clear
  }
}
class ConnectionHolder(val e: Edge, val container: ContainerItem) {
  lazy val painter = new ConnectionPainter(container)
  var feedback = false
  def paint = painter.paintRoute(e, feedback)
  paint
  def destroy() {
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
  // FIXME selction subject for selection from error
}
