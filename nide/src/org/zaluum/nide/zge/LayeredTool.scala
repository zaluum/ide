package org.zaluum.nide.zge
import scala.annotation.implicitNotFound
import scala.reflect.Manifest.singleType

import org.eclipse.draw2d.Cursors
import org.eclipse.draw2d.Figure
import org.eclipse.swt.graphics.Cursor
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.Vector2

import draw2dConversions._
import RichFigure._
abstract class LayeredTool(viewer: ItemViewer) extends Tool(viewer) {
  type C <: ContainerItem
  def itemUnderMouse = current.itemAt(point(currentMouseLocation), false)
  def currentMouseLocation: Point = current.translateFromViewport(absMouseLocation)
  def current: C = viewer.findContainerAt(point(absMouseLocation)).asInstanceOf[C]
  abstract class OverTrack[F <: Figure](implicit m: Manifest[F]) {
    var current: Option[F] = None
    protected var last: Option[F] = None
    val partial: PartialFunction[AnyRef, F] = { case s if singleType(s) <:< m ⇒ s.asInstanceOf[F] }
    def update() {
      val near = viewer.deepChildrenNear(point(absMouseLocation), 10)
      val fil = for ((f, d) ← near; if (partial.isDefinedAt(f))) yield (partial(f), d)
      val under = if (fil.isEmpty) None
      else Some(fil.minBy(_._2)._1)
      if (under == last) return ;
      last foreach { f ⇒ last = None; onExit(f); }
      under foreach { f ⇒ last = Some(f); onEnter(f); }
    }
    def onEnter(f: F) { current = last }
    def onExit(f: F) { current = None }
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
    def currentMouseLocation = initContainer.translateFromViewport(absMouseLocation)
  }
  trait DeltaMove extends ToolState {
    private var _initDrag: Point = _
    def enterMoving(initDrag: Point) {
      this._initDrag = initDrag
    }
    def currentMouseLocation: Point
    protected def delta = {
      val now = currentMouseLocation
      Vector2(now.x - _initDrag.x, now.y - _initDrag.y)
    }
    def initDrag = _initDrag
  }
}