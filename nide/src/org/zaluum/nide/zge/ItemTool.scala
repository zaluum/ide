package org.zaluum.nide.zge

import org.zaluum.nide.compiler.EditTransformer
import draw2dConversions._
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ Cursors, Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Point => EPoint, Rectangle }
import org.zaluum.nide.compiler.{  _ }
import scala.collection.JavaConversions._
import scala.reflect.Manifest._
import FigureHelper._

/**
 * Implements basic selecting, marquee and resizing of ItemFigures
 * @author frede
 *
 */
abstract class ItemTool(viewer: ItemViewer) extends LayeredTool(viewer) {
  def selecting : Selecting
  type C = ContainerItem
  state = selecting
  // SELECTING 
  abstract class Selecting extends ToolState {
    var beingSelected: Option[Item] = None
    var initDrag: Point = _
    var initContainer: C = _
    def enter() { state = this; }

    def buttonDown {
      beingSelected = itemUnderMouse
      initDrag = currentMouseLocation
      initContainer = current
    }

    val handleTrack = new OverTrack[HandleRectangle] {
      def container = viewer.feedbackLayer
      override def onEnter(h: HandleRectangle) {
        super.onEnter(h)
        h.setXOR(true);
        viewer.setCursor(h.resizeCursor)
      }
      override def onExit(h: HandleRectangle) {
        super.onExit(h)
        h.setXOR(false);
        viewer.setCursor(null)
      }
    }
    def move {
      handleTrack.update()
    }
    
    def exit {}
    def abort {}
  }
  //// MOVE
    
  /// MARQUEE
  object marqueeing extends DeltaMove with SingleContainer {
    def enter(p: Point, initContainer: C) {
      enterSingle(initContainer)
      enterMoving(p)
      state = this
      viewer.setCursor(Cursors.CROSS)
      viewer.showMarquee()
      move()
    }
    def exit() {
      viewer.setCursor(null)
      selecting.enter()
    }
    def abort() { exit() }
    def buttonDown {}
    def drag {}
    def buttonUp {
      viewer.hideMarquee()
      exit()
    }
    override def move { viewer.moveMarquee(new Rectangle(point(currentMouseLocation), point(Point(0,0)))) } // FIXME
  }
  // RESIZING
  val resizing  = new Resizing
  class Resizing extends DeltaMove with SingleContainer {
    var handle: HandleRectangle = _
    def itf = handle.resizeItemFigure
    def enter(initDrag: Point, initContainer: C, handle: HandleRectangle) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      this.handle = handle
      state = this
    }
    def buttonUp {
      val newBounds = handle.deltaAdd(delta, itf.getBounds);
      val newPos = newBounds.getLocation
      val newSize = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(15, 15))
      itf match {
        case vd: ValDefItem =>
          controller.exec(command(newPos, newSize,vd.valDef))
        case _ => abort() 
      }
    }
    def command(newPos: Point, newSize: Dimension,t:Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v:ValDef if (v == t) ⇒
          v.copy(size=Some(newSize), pos=newPos)
      }
    }
    def move() { itf.resizeDeltaFeed(delta, handle) }
    def abort() {
      itf.resizeDeltaFeed(Vector2(0, 0), handle)
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
  }

}
