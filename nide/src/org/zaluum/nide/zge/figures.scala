package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{FreeformLayer, Ellipse, ColorConstants, Figure, ImageFigure, Polyline}
import org.eclipse.draw2d.geometry.{Rectangle, Point => EPoint, Dimension => EDimension}
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.model._

object draw2dConversions {
  implicit def point(p:Point) : EPoint = new EPoint(p.x,p.y)
  implicit def dimension(d:Dimension) : EDimension = new EDimension(d.w,d.h)
  implicit def rpoint(p:EPoint) : Point = Point(p.x,p.y)
  implicit def rdimension(d:EDimension) : Dimension = Dimension(d.width,d.height)
}
trait CanShowFeedback extends Figure {
  def showFeedback()
  def hideFeedback()
}
trait CanShowUpdate extends Figure {
  def show()
  def hide()
  def update()
}
trait Selectable extends Figure with CanShowFeedback 
trait ItemFigure extends Selectable with CanShowUpdate {
  def viewer: Viewer
  def positionable : Positionable
  def feed : ItemFeedbackFigure
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
trait ResizableItemFigure extends ItemFigure {
  def feed : ResizeItemFeedbackFigure
  def resizable : Resizable
}
trait ItemFigureWithPorts extends ItemFigure {
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
trait BoxFigure extends ItemFigureWithPorts{
  def box:Box
  var boxClass: Option[BoxClass]
  def positionable = box
  lazy val feed = new ItemFeedbackFigure(this)
  object portMapper extends ModelViewMapper[TypedPort, PortFigure] {
    def modelSet = boxClass.map { _.ports } getOrElse Set()
    def buildFigure(p: TypedPort) = new PortFigure(BoxFigure.this, p, BoxPortRef(box, p.name), viewer)
  }
}
object PortDeclFigure {
  def img(in:Boolean) = "org/zaluum/nide/icons/portDecl" + (if(in) "In" else "Out") + ".png"
}
class PortDeclFigure(val portDecl:PortDecl, val viewer:Viewer) extends ImageFigure with ItemFigureWithPorts {
  def positionable = portDecl
  var size = Dimension(50,20)
  lazy val feed = new ItemFeedbackFigure(this)
  def position = if (portDecl.in) Point(48,8) else Point(0,8) 
  def typedPort = TypedPort(portDecl.descriptor,portDecl.in,portDecl.name,position)

  object portMapper extends ModelViewMapper[TypedPort, PortFigure] {
    def modelSet = Set(typedPort)
    def buildFigure(p: TypedPort) = new PortFigure(PortDeclFigure.this, p, ModelPortRef(p.name), viewer)
  }
  override def update(){
    val image = viewer.imageFactory.get(PortDeclFigure.img(portDecl.in)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    super.update()
  }
}

class PortFigure(val bf: ItemFigureWithPorts, val typ: TypedPort, val portRef: PortRef, viewer: Viewer) extends Ellipse with CanShowFeedback with CanShowUpdate {
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
    val dx = typ.pos.x
    val dy = typ.pos.y
    val x = bf.getBounds.x + dx - getBounds.width/2
    val y = bf.getBounds.y + dy - getBounds.height/2
    setLocation(Point(x, y))
  }
}

class ImageBoxFigure(val box: Box, var boxClass: Option[BoxClass], val viewer: Viewer) extends ImageFigure with BoxFigure {
  setImage(viewer.imageFactory(boxClass))
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
}

class LineFigure(l: Line, val cf: ConnectionFigure, modelView: ModelView) extends Polyline with CanShowUpdate with Selectable{
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
class ConnectionFigure(val c: Connection, modelView: ModelView) extends Figure with CanShowUpdate {
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