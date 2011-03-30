package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ Cursors, Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Point => EPoint, Rectangle }
import org.zaluum.nide.compiler.{_ }
import scala.collection.JavaConversions._
import scala.reflect.Manifest._
import RichFigure._
abstract class LayeredTool(viewer: ItemViewer) extends Tool(viewer) {
  type C <: Container
  def itemOrLineUnderMouse = current.itemAt(point(currentMouseLocation))
  def currentMouseLocation : Point = translate(current, absMouseLocation)
  def current : C = viewer.findDeepContainerAt(point(absMouseLocation)) {
    case (f: OpenBoxFigure) ⇒ f.asInstanceOf[C]
  } getOrElse { viewer.asInstanceOf[C] }
  def translate(me: IFigure, p: Point): Point = {
    if (me eq viewport) p
    else {
      val ep = translate(me.getParent, p)
      me.translateFromParent(point(ep))
      ep
    }
  }
  abstract class OverTrack[F <: Figure](implicit m: Manifest[F]) {
    var last: Option[F] = None
    val partial : PartialFunction[AnyRef,F] = { case s if singleType(s)<:<m => s.asInstanceOf[F]}
    def update() {
      val near = viewer.deepChildrenNear(point(absMouseLocation),10)
      val fil= for ((f,d) <- near; if (partial.isDefinedAt(f))) yield (partial(f),d)
      val under = if (fil.isEmpty)  None
        else Some(fil.minBy( _._2)._1)
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
  trait SingleContainerAllower extends SingleContainer with Allower {
    def allowed = initContainer eq current
  }
  trait SingleContainer extends ToolState {
    var initContainer: C = _
    def enterSingle(initContainer: C) {
      this.initContainer = initContainer
    }
    def currentMouseLocation = translate(initContainer, absMouseLocation)
  }
  trait DeltaMove extends ToolState {
    private var initDrag: Point = _
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