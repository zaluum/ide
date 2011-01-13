package org.zaluum.nide.zge

import org.eclipse.draw2d.Polyline
import org.eclipse.swt.graphics.Image
import org.eclipse.draw2d.ImageFigure
import org.eclipse.draw2d.GridLayout
import org.eclipse.draw2d.FreeformLayout
import org.eclipse.draw2d.XYLayout
import org.eclipse.draw2d.FlowLayout
import scala.collection.mutable.Buffer
import javax.swing.UIManager
import javax.swing.JComponent
import javax.swing.JButton
import javax.swing.JPanel
import org.eclipse.swt.SWT
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.IFigure
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, FreeformViewport, LightweightSystem, Ellipse, ColorConstants, Figure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point => EPoint, Dimension=>EDimension }
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.model._

object draw2dConversions {
  implicit def point(p:Point) : EPoint = new EPoint(p.x,p.y)
  implicit def dimension(d:Dimension) : EDimension = new EDimension(d.w,d.h)
  implicit def rpoint(p:EPoint) : Point = Point(p.x,p.y)
  implicit def rdimension(d:EDimension) : Dimension = Dimension(d.width,d.height)
}
import draw2dConversions._
trait CanShowFeedback extends Figure {
  def showFeedback()
  def hideFeedback()
}
trait CanShowUpdate extends Figure {
  def show()
  def hide()
  def update()
}

trait BasicFigure extends Figure with CanShowFeedback with CanShowUpdate {
  def viewer: Viewer
  def positionable : Positionable
  def feed : BasicFeedbackFigure
  def size: Dimension
  def showFeedback() {
    viewer.feedbackLayer.add(feed)
    update()
  }
  def hideFeedback {
    if (viewer.feedbackLayer.getChildren.contains(feed))
      viewer.feedbackLayer.remove(feed)
  }
  def show() {
    viewer.layer.add(this)
    update()
  }
  def hide() {
    if (viewer.layer.getChildren.contains(this))
      viewer.layer.remove(this)
  }
  def update() {
    val rect = new Rectangle(positionable.pos.x, positionable.pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  def moveFeed(loc : Point) {
    feed.setInnerLocation(loc)
  }
  def moveDeltaFeed(delta: Vector2) {
    val loc = positionable.pos + delta 
    feed.setInnerLocation(loc)
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {
    feed.setInnerBounds(handle.deltaAdd(delta, getBounds))
  }
}
trait FigureWithPorts extends BasicFigure {
  def portMapper : ModelViewMapper[TypedPort, PortFigure] ;
  def find(name: String) = portMapper.values.find { _.typ.name == name }
  override def show() {
    super.show()
    portMapper.viewMap.values foreach { _.show() }
  }
  override def hide() {
    super.hide()
    portMapper.viewMap.values foreach { _.hide() }
  }
  override def update() {
    super.update()
    portMapper.update()
  }
}
trait BoxFigure extends FigureWithPorts{
  def box:Box
  var boxClass: Option[BoxClass]
  def positionable = box
  lazy val feed = new BasicFeedbackFigure(this)
  object portMapper extends ModelViewMapper[TypedPort, PortFigure] {
    def modelSet = boxClass.map { _.ports } getOrElse Set()
    def buildFigure(p: TypedPort) = new PortFigure(BoxFigure.this, p, PortRef(box, p.name), viewer)
  }
}
class PortDeclFigure(val portDecl:PortDecl, val viewer:Viewer,var portType : Option[TypedPort]) extends RectangleFigure with FigureWithPorts {
  def positionable = portDecl
  def size = Dimension(50,20)
  lazy val feed = new BasicFeedbackFigure(this)
  object portMapper extends ModelViewMapper[TypedPort, PortFigure] {
    def modelSet = portType.toSet
    def buildFigure(p: TypedPort) = new PortFigure(PortDeclFigure.this, p, PortRef(null/*FIXME*/, p.name), viewer)
  }
}
class PortFigure(val bf: FigureWithPorts, val typ: TypedPort, val portRef: PortRef, viewer: Viewer) extends Ellipse with CanShowFeedback with CanShowUpdate {
  setAntialias(1)
  setAlpha(50)
  setOutline(false)
  val highlight = ColorConstants.blue
  val normal = ColorConstants.gray
  setBackgroundColor(normal)
  def show() {
    viewer.portsLayer.add(this)
    update()
  }
  def hide() {
    if (viewer.portsLayer.getChildren.contains(this))
      viewer.portsLayer.remove(this)
  }
  def showFeedback() {
    setBackgroundColor(highlight)
  }
  def hideFeedback() {
    setBackgroundColor(normal)
  }
  def anchor = getBounds.getCenter
  def update() {
    setSize(10, 10)
    val dx = typ.pos._1
    val dy = typ.pos._2
    val x = bf.getBounds.x + dx - getBounds.width/2
    val y = bf.getBounds.y + dy - getBounds.height/2
    setLocation(Point(x, y))
  }
}

class ImageBoxFigure(val box: Box, var boxClass: Option[BoxClass], val viewer: Viewer, val image: Image) extends ImageFigure(image) with BoxFigure {
  
  def size = Dimension(image.getBounds.width, image.getBounds.height)
}
class SwingBoxFigure(val viewer: Viewer, val box: Box, c: JComponent) extends SwingFigure(c) with BoxFigure {
  def size = Dimension(50, 50) //FIXME
  var boxClass: Option[BoxClass] = None // FIXME
}

import org.eclipse.draw2d.Polyline

class LineFigure(l: Line, val pl: ConnectionFigure, modelView: ModelView) extends Polyline with CanShowUpdate with CanShowFeedback {
  //setAntialias(1)
  setForegroundColor(ColorConstants.gray)
  def hide() = modelView.viewer.connectionsLayer.remove(this)
  def show() = modelView.viewer.connectionsLayer.add(this)
  var complete = false
  var feedback = false
  def update() {
    setStart(new Point(l.from.x, l.from.y))
    setEnd(new Point(l.end.x, l.end.y))
  }
  def showFeedback { feedback = true; calcStyle }
  def hideFeedback { feedback = false; calcStyle }
  def showComplete { complete = true; calcStyle }
  def showIncomplete { complete = false; calcStyle }
  def calcStyle {
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
}
class ConnectionFigure(c: Connection, modelView: ModelView) extends Figure with CanShowUpdate {
  object lines extends ModelViewMapper[Line, LineFigure] {
    def buildFigure(l: Line) = new LineFigure(l, ConnectionFigure.this, modelView)
    def modelSet = c.buf.toSet
  }
  def show() = lines.viewMap.values foreach { _.show() }
  def hide() = lines.viewMap.values foreach { _.hide() }
  def fromFig = c.from flatMap { f ⇒ modelView.findPortFigure(f) }
  def toFig = c.to flatMap { t ⇒ modelView.findPortFigure(t) }
  def withFullConnection(body: (PortFigure, PortFigure) ⇒ Unit) {
    (fromFig, toFig) match {
      case (Some(f), Some(t)) ⇒ body(f, t)
      case _ ⇒
    }
  }
  def updateStarts() { // FIXME move to command?
    if (c.buf.isEmpty) {
      withFullConnection { (fromFig, toFig) ⇒
        c.simpleConnect(fromFig.anchor, toFig.anchor)
      }
    } else {
      withFullConnection { (fromFig, toFig) ⇒
        if (rpoint(fromFig.anchor) != c.buf.head.from || rpoint(toFig.anchor) != c.buf.last.end){
          c.simpleConnect(fromFig.anchor, toFig.anchor) // TODO only move
        }
      }
    }
  }
  def update() = {
    updateStarts()
    lines.update()
    if (c.from.isDefined && c.to.isDefined)
      lines.values.foreach { _.showComplete }
    else
      lines.values.foreach { _.showIncomplete }
  }

}