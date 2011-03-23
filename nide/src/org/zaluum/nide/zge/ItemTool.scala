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
  lazy val selecting = new Selecting
  type C <: Container
  state = selecting
  // SELECTING 
  class Selecting extends ToolState {
    var beingSelected: Option[Item] = None
    var handle: Option[HandleRectangle] = None
    var initDrag: Point = _
    var initContainer: C = _
    def enter() { state = this; }

    def buttonDown {
      beingSelected = itemOrLineUnderMouse collect { case i: Item ⇒ i }
      initDrag = currentMouseLocation
      initContainer = current
    }

    def buttonUp {
      beingSelected match {
        case Some(s:Item) => viewer.selection.updateSelection(s.selectionSubject.toSet, shift)
        case None ⇒ viewer.selection.deselectAll()
      }
      viewer.refresh()
    }

    val handleTrack = new OverTrack[HandleRectangle] {
      def container = viewer.feedbackLayer
      def onEnter(h: HandleRectangle) {
        handle = Some(h)
        h.setXOR(true);
        viewer.setCursor(h.resizeCursor)
      }
      def onExit(f: HandleRectangle) {
        handle = None
        f.setXOR(false);
        viewer.setCursor(null)
      }
    }
    def move {
      handleTrack.update()
    }
    def drag {
      (handle, beingSelected) match {
        case (Some(h), _) ⇒ // resize
          resizing.enter(initDrag, initContainer, h)
        case (None, Some(fig)) ⇒ // select and move
          if (fig.selectionSubject.isDefined){
            if (!viewer.selection(fig.selectionSubject.get)){
              viewer.selection.updateSelection(fig.selectionSubject.toSet, shift)
              fig.showFeedback()
            }
          }
          moving.enter(initDrag, initContainer)
        case (None, None) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    def exit {}
    def abort {}
  }
  // MOVE
  trait Moving extends ToolState {
    self: DeltaMove with SingleContainer ⇒
    def enter(initDrag: Point, initContainer: C) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      state = this
    }

    def allowed = (current eq initContainer) || (movables.exists { isOrHas(_, current) })
    def movables = viewer.selectedItems.collect {
      case item:TreeItem if item.container == initContainer ⇒ item
    }
    def buttonUp {
      val positions = movables.map { item ⇒
        val oldLoc = item.getBounds.getLocation
        (item.tree.asInstanceOf[Tree] -> (Point(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) if (positions.contains(v)) ⇒
            ValDef(name, typeName, positions(v), size, guiPos, guiSize,params)
          case p: PortDef if (positions.contains(p)) ⇒
            p.copy(inPos = positions(p))
        }
      }
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { viewer.selectedItems foreach { _.moveDeltaFeed(delta) } }
    def abort() {
      viewer.selectedItems foreach { _.moveDeltaFeed(Vector2(0, 0)) }
      exit()
    }
  }
  class MovingItem extends Moving with DeltaMove with SingleContainer with Allower 
  val moving = new MovingItem
  
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
        case ti: TreeItem =>
          controller.exec(command(newPos, newSize,ti.tree))
        case _ =>  
      }
    }
    def command(newPos: Point, newSize: Dimension,t:Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v:ValDef if (v == t) ⇒
          v.copy(size=Some(newSize))
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
