package org.zaluum.nide.zge

import org.eclipse.swt.events.FocusListener
import org.eclipse.jface.viewers.ICellEditorListener
import org.eclipse.swt.widgets.Text
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.draw2d.text.TextFlow
import org.eclipse.draw2d.text.FlowPage
import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{ ColorConstants, Figure, ImageFigure, Polyline }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler.{ Point ⇒ MPoint, _ }
import scala.collection.mutable.Buffer

// TREE SPECIFIC FIGURES
trait ValFigure extends SimpleItem with TreeItem {
  type T = ValDef
  def sym = tree.symbol.asInstanceOf[ValSymbol]
  def pos = tree.pos
  def myLayer = container.layer
  def container : BoxDefContainer
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
class ImageValFigure(val tree: ValDef, val container: BoxDefContainer) extends ImageFigure with ValFigure  with RectFeedback {
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
  override def update() {
    setImage(container.viewerResources.imageFactory(tree.tpe))
    super.update()
  }
}
class DirectValFigure(val tree: ValDef, val param:Param, val container: BoxDefContainer) extends TextEditFigure with ValFigure {
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
class LineFigure(l: Line, bdf: BoxDefContainer, val con: Option[ConnectionDef] = None) extends Polyline with Selectable {
  //setAntialias(1)
  //setForegroundColor(ColorConstants.gray)
  var complete = false
  var feedback = false
  showComplete
  def showFeedback { feedback = true; calcStyle }
  def hideFeedback { feedback = false; calcStyle }
  def showComplete { complete = true; calcStyle }
  def showIncomplete { complete = false; calcStyle }
  def calcStyle {
    setForegroundColor(Colorizer.color(con map { _.tpe } getOrElse NoSymbol))
    if (feedback) {
      setLineStyle(SWT.LINE_DASH)
      setLineWidth(2)
    } else {
      setLineWidth(1)
      if (complete) {
        setLineStyle(SWT.LINE_SOLID)
      } else {
        setLineStyle(SWT.LINE_DOT)
      }
    }
  }
  def show() {
    setStart(new EPoint(l.from.x, l.from.y))
    setEnd(new EPoint(l.end.x, l.end.y))
    calcStyle
    bdf.connectionsLayer.add(this)
  }
  def hide() {
    if (bdf.connectionsLayer.getChildren.contains(this))
      bdf.connectionsLayer.remove(this)
  }
}
class ConnectionPainter(bdf: BoxDefContainer) {
  val lines = Buffer[LineFigure]()
  def paintRoute(route: Route, feedback: Boolean, con: Option[ConnectionDef] = None) {
    clear()
    route.lines foreach { l ⇒ lines += new LineFigure(l, bdf, con) }
    lines foreach { l ⇒ if (feedback) l.showFeedback() else l.hideFeedback(); l.show }
  }
  def clear() {
    lines.foreach { _.hide }
    lines.clear
  }
}
// TODO not really a figure right now... no children
class ConnectionFigure(val tree: ConnectionDef, val container: BoxDefContainer) extends TreeItem {
  type T = ConnectionDef
  val painter = new ConnectionPainter(container)
  def calcRoute = {
    def portFigure(tree: Tree): Option[PortFigure] = tree match {
      case PortRef(v@ValRef(_), portName, in) ⇒ container.findPortFigure(v.symbol.name, portName, in)
      case PortRef(ThisRef, portName, in) ⇒ container.findPortFigure(portName, in)
      case _ ⇒ None
    }
    def position(tree: Tree): Option[MPoint] = portFigure(tree) map { p ⇒ p.anchor }
    val aw = position(tree.a).map (p=> Waypoint(p,H)).toList
    val bw = position(tree.b).map (p=> Waypoint(p,H)).toList
    Route(bw ::: tree.wayPoints ::: aw)
  }
  var feedback = false
  def paint = painter.paintRoute(calcRoute,feedback, Some(tree))
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
  def moveFeed(p: MPoint) {}
}
