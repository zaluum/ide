package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ Cursors, Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import org.zaluum.nide.newcompiler.{ Transformer, Tree, ValDef, CopyTransformer, PortDef }
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
  state = selecting
  // SELECTING 
  class Selecting extends ToolState {
    var selected: Option[Item] = None
    var lineSelected: Option[LineFigure] = None
    var handle: Option[HandleRectangle] = None
    var port: Option[PortFigure] = None
    var initDrag: Point = _
    var initContainer: BoxDefContainer = _
    def enter() { state = this; }
    def buttonDown {
      selected = figureUnderMouse
      if (selected.isEmpty) lineSelected = lineUnderMouse
      initDrag = currentMouseLocation.getCopy
      initContainer = current
    }

    def buttonUp {
      (selected, lineSelected) match {
        case (Some(box), _) ⇒ viewer.selected.updateSelection(Set(box), shift)
        case (None, Some(line)) ⇒ viewer.selected.updateSelection(Set(line), shift)
        case (None, None) ⇒ viewer.deselectAll()
      }
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
    val portsTrack = new OverTrack[PortFigure] {
      def container = viewer.portsLayer
      def onEnter(p: PortFigure) { port = Some(p); p.showFeedback }
      def onExit(p: PortFigure) { port = None; p.hideFeedback }
    }
    def move {
      handleTrack.update()
      portsTrack.update()
    }
    def drag {
      (handle, selected, port) match {
        case (Some(h), _, _) ⇒ // resize
          resizing.enter(initDrag, initContainer, h)
        case (None, _, Some(port)) ⇒ // connect
          connect(port)
        case (None, Some(fig), _) ⇒ // select and move
          if (!viewer.selected(fig))
            viewer.selected.updateSelection(Set(fig), shift)
          fig match {
            case oPort: OpenPortDeclFigure ⇒ movingOpenPort.enter(initDrag, initContainer, oPort)
            case _ ⇒ moving.enter(initDrag, initContainer)
          }
        case (None, None, None) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    def connect(port: PortFigure) {}
    def exit {}
    def abort {}
  }
  // MOVE
  trait Moving extends ToolState {
    self: DeltaMove with SingleContainer ⇒
    def enter(initDrag: Point, initContainer: BoxDefContainer) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      state = this
    }

    def allowed = (current eq initContainer) || (movables.exists { isOrHas(_, current) })
    def movables = viewer.selected.selected.collect {
      case item: Item if item.container == initContainer ⇒ item
    }
    def buttonUp {
      val positions = movables.map { item ⇒
        val oldLoc = item.getBounds.getLocation
        (item.tree.asInstanceOf[Tree] -> (MPoint(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      val command = TreeCommand(new CopyTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize) if (positions.contains(v)) ⇒
            ValDef(name, typeName, positions(v), size, guiPos, guiSize)
          case p: PortDef if (positions.contains(p)) ⇒
            p.copy(inPos = positions(p))
        }
      })
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { viewer.selected.selected collect { case bf: Item ⇒ bf } foreach { _.moveDeltaFeed(delta) } }
    def abort() {
      viewer.selected.selected collect { case bf: Item ⇒ bf } foreach { _.moveDeltaFeed(Vector2(0, 0)) }
      exit()
    }
  }
  object moving extends Moving with DeltaMove with SingleContainer with Allower
  def movingOpenPort: { def enter(initDrag: Point, initContainer: BoxDefContainer, fig: OpenPortDeclFigure) }

  /// MARQUEE
  object marqueeing extends DeltaMove with SingleContainer {
    def enter(p: Point, initContainer: BoxDefContainer) {
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
    override def move { viewer.moveMarquee(new Rectangle(currentMouseLocation, initDrag)) } // FIXME
  }
  // RESIZING
  object resizing extends DeltaMove with SingleContainer {
    var handle: HandleRectangle = _
    def itf = handle.resizeItemFigure
    def enter(initDrag: Point, initContainer: BoxDefContainer, handle: HandleRectangle) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      this.handle = handle
      state = this
    }
    def buttonUp {
      val newBounds = handle.deltaAdd(delta, itf.getBounds);
      val newPos = newBounds.getLocation
      val newSize = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(15, 15))
      val command = TreeCommand(new CopyTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize) if (v == itf.tree) ⇒
            ValDef(name, typeName, newPos, Some(newSize), guiPos, guiSize)
        }
      })
      controller.exec(command)
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
