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

// TREE SPECIFIC FIGURES
trait ValFigure extends SimpleItem with TreeItem {
  type T = ValDef
  def sym = tree.symbol.asInstanceOf[ValSymbol]
  def pos = tree.pos
  def myLayer = container.layer
  def container: BoxDefContainer
  override def update() {
    super.update()
    val l = sym.tpe match {
      case b: BoxTypeSymbol ⇒
        b.ports.values.collect {
          case s: PortSymbol ⇒
            new PortFigure(s.extPos + Vector2(getBounds.x, getBounds.y),
              s, s.dir == In, Some(sym), container)
        }.toList
      case _ ⇒ List()
    }
    helpers.appendAll(l);
  }
}
class ImageValFigure(val tree: ValDef, val container: BoxDefContainer) extends ImageFigure with ValFigure with RectFeedback {
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
  override def update() {
    setImage(container.viewerResources.imageFactory(tree.tpe))
    super.update()
  }
}
class DirectValFigure(val tree: ValDef, val param: Param, val container: BoxDefContainer) extends TextEditFigure with ValFigure {
  def size = Dimension(40, 20)
  def text = param.value
  override def update() {
    fl.setText(text)
    setForegroundColor(Colorizer.color(param.tpe))
    super.update()
  }
}
trait TextEditFigure extends RectangleFigure with SimpleItem with RectFeedback {
  def text: String;

  val pg = new FlowPage()
  pg.setForegroundColor(ColorConstants.black)
  pg.setBounds(new Rectangle(2, 2, 40, 20))
  val fl = new TextFlow()
  pg.add(fl)
  add(pg)

  var textCellEditor: TextCellEditor = null
  def edit(onComplete: (String) ⇒ Unit, onCancel: () ⇒ Unit) = {
    if (textCellEditor == null) {
      textCellEditor = new TextCellEditor(container.viewer.canvas)
      val textC = textCellEditor.getControl().asInstanceOf[Text]
      textC.setText(text)
      textCellEditor.activate()
      textCellEditor.addListener(new ICellEditorListener() {
        def applyEditorValue() { onComplete(textC.getText) }
        def cancelEditor() { onCancel() }
        def editorValueChanged(oldValid: Boolean, newValid: Boolean) {}
      })
      val b = getClientArea.getCopy
      translateToAbsolute(b)
      textC.setBounds(b.x + 1, b.y + 1, b.width - 2, b.height - 2)
      textC.setBackground(ColorConstants.white)
      textC.setVisible(true)
      textC.selectAll()
      textC.setFocus
    }
  }
  def hideEdit() = {
    if (textCellEditor != null) {
      textCellEditor.dispose()
      textCellEditor = null
    }
  }
}
case class LineSelectionSubject(c: ConnectionDef, l: Line) extends SelectionSubject
class LineFigure(
  val l: Line,
  val r: Edge,
  val container: BoxDefContainer,
  val complete:Boolean,
  val con: Option[ConnectionFigure] = None) extends SimpleItem with RectFeedback {
  val tolerance = 4
  def expand = ((width + 2) / 2.0f).asInstanceOf[Int]
  override def selectionSubject = con map { cf ⇒ LineSelectionSubject(cf.tree, l) }
  override def getBounds: Rectangle = {
    if (bounds == null) {
      val (expandx, expandy) = if (l.horizontal) (0, expand) else (expand, 0)
      bounds = new Rectangle(point(l.start), point(l.end)).expand(expandx, expandy)
    }
    bounds
  }
  def size = getBounds.getSize //special size
  def pos = getBounds.getLocation
  override def containsPoint(x: Int, y: Int) = {
    val t = math.max(expand, tolerance).asInstanceOf[Int];
    val b = getBounds.getCopy
    b.expand(t, t)
    b.contains(x, y)
    //  return shapeContainsPoint(x, y) || childrenContainsPoint(x, y);
  }
  override def update = {
    setForegroundColor(Colorizer.color(con map { _.tree.tpe } getOrElse NoSymbol))
    width = 1
    if (complete) {
      style = SWT.LINE_SOLID
    } else {
      style = SWT.LINE_DOT
    }
    helpers.clear
    //erase
    //repaint
    feed.setInnerBounds(getBounds)
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

class PointFigure(p: Point, val container: BoxDefContainer, color: Color) extends Ellipse with Item {
  def show() {
    setSize(6, 6)
    setFill(true)
    setLocation(point(p + Vector2(-3, -3)))
    setBackgroundColor(color); // if (p.d == H) ColorConstants.yellow else ColorConstants.blue ) 
    container.connectionsLayer.add(this)
  }
  def showFeedback() {}
  def hideFeedback() {}
  def hide() {
    if (container.connectionsLayer.getChildren.contains(this))
      container.connectionsLayer.remove(this)
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
class ConnectionFigure(val tree: ConnectionDef, val container: BoxDefContainer) extends TreeItem {
  type T = ConnectionDef
  val painter = new ConnectionPainter(container)
  def route = {
    /*def portFigure(tree: Tree): Option[PortFigure] = tree match {
      case PortRef(v@ValRef(_), portName, in) ⇒ container.findPortFigure(v.symbol.name, portName, in)
      case PortRef(ThisRef, portName, in) ⇒ container.findPortFigure(portName, in)
      case _ ⇒ None
    }
    def position(tree: Tree): Option[Point] = portFigure(tree) map { p ⇒ p.anchor }
    val aw = position(tree.a).map(p ⇒ Waypoint(p, H)).toList
    val bw = position(tree.b).map(p ⇒ Waypoint(p, H)).toList
    Route(bw ::: tree.wayPoints ::: aw)*/
    new Edge(new Joint(tree.wayPoints.head.p), new Joint(tree.wayPoints.last.p), tree.wayPoints map { _.p })
  }
  var feedback = false
  def paint = painter.paintRoute(route, feedback, true, Some(this))
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
