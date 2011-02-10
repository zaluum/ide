package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ Cursors, Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import org.zaluum.nide.newcompiler.{ Transformer, Tree, ValDef, CopyTransformer, PortDef }
import scala.collection.JavaConversions._
import scala.reflect.Manifest._
import RichFigure._
abstract class LayeredTool(viewer: ItemViewer) extends Tool(viewer) {
  def figureUnderMouse = current.itemAt(currentMouseLocation,true)
  def lineUnderMouse = current.lineAt(currentMouseLocation)
  def currentMouseLocation = translate(current, absMouseLocation)
  def current = viewer.findDeepContainerAt(absMouseLocation) {
    case (f: OpenBoxFigure) ⇒ f
  } getOrElse { viewer }
  def translate(me: IFigure, p: Point): Point = {
    if (me eq viewport) p
    else {
      val ep = translate(me.getParent, p.getCopy)
      me.translateFromParent(ep)
      ep
    }
  }
  abstract class OverTrack[F <: Figure](implicit m: Manifest[F]) {
    def container: IFigure
    var last: Option[F] = None
    val partial : PartialFunction[AnyRef,F] = { case s if singleType(s)<:<m => s.asInstanceOf[F]}
    def update() {
      val under: Option[F] = viewer.findDeepAt(absMouseLocation)(partial)
      if (under == last) return ;
      last foreach { f ⇒ onExit(f); last = None }
      under foreach { f ⇒ onEnter(f); last = Some(f) }
    }
    def onEnter(f: F)
    def onExit(f: F)
  }
  trait Allower extends ToolState {
    private var storedCursor: Option[Cursor] = None
    def allowed: Boolean
    abstract override def move() {
      if (allowed) {
        if (storedCursor.isDefined) {
          storedCursor foreach { viewer.setCursor(_) }
          storedCursor = None
        }
      } else {
        if (storedCursor == None) {
          storedCursor = Some(viewer.getCursor)
          viewer.setCursor(Cursors.NO)
        }
      }
      super.move()
    }
    abstract override def buttonUp() =
      if (allowed) super.buttonUp else abort()
    abstract override def abort() = {
      storedCursor.foreach { viewer.setCursor(_) }
      super.abort()
    }
  }
  trait SingleContainer extends ToolState {
    var initContainer: BoxDefContainer = _
    def enterSingle(initContainer: BoxDefContainer) {
      this.initContainer = initContainer
    }
    def currentMouseLocation = translate(initContainer, absMouseLocation)
    def allowed = initContainer eq current
  }
  trait DeltaMove extends ToolState {
    var initDrag: Point = _
    def enterMoving(initDrag: Point) {
      this.initDrag = initDrag
    }
    def currentMouseLocation: Point
    protected def delta = {
      val now = currentMouseLocation
      Vector2(now.x - initDrag.x, now.y - initDrag.y)
    }
  }
}