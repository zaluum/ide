package org.zaluum.nide.zge

import org.zaluum.nide.newcompiler.CopyTransformer
import org.zaluum.nide.newcompiler.ValDef
import org.zaluum.nide.newcompiler.Tree
import org.zaluum.nide.newcompiler.Transformer
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import scala.collection.JavaConversions._

abstract class AbstractTool(viewer: AbstractViewer) extends Tool(viewer) {
  def modelView = viewer.modelView

  lazy val selecting = new Selecting
  state = selecting
  // SELECTING 
  class Selecting extends ToolState {
    var selected: Option[ItemFigure] = None
    var lineSelected: Option[LineFigure] = None
    var handle: Option[HandleRectangle] = None
    var port: Option[PortFigure] = None
    var initDrag: Point = _
    def enter() { state = this; }
    def buttonDown {
      selected = figureUnderMouse
      if (selected.isEmpty) lineSelected = lineUnderMouse
      initDrag = mouseLocation.getCopy
    }

    def buttonUp {
      (selected, lineSelected) match {
        case (Some(box), _) ⇒ modelView.selected.updateSelection(Set(box), shift)
        case (None, Some(line)) ⇒ modelView.selected.updateSelection(Set(line), shift)
        case (None, None) ⇒ modelView.deselectAll()
      }
    }
    val handleTrack = new OverTrack[HandleRectangle](viewer.feedbackLayer) {
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
    val portsTrack = new OverTrack[PortFigure](viewer.portsLayer) {
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
          resizing.enter(initDrag, h)
        case (None, _, Some(port)) ⇒ // connect
          connect(port)
        case (None, Some(fig), _) ⇒ // select and move
          if (!modelView.selected(fig))
            modelView.selected.updateSelection(Set(fig), shift)
          moving.enter(initDrag)
        case (None, None, None) ⇒ marqueeing.enter(initDrag) // marquee
      }
    }
    def connect(port: PortFigure) {}
    def abort {}
    def exit {}
  }
  // MOVE
  object moving extends MovingState {
    def doEnter {}
    def buttonUp {
      val positions = modelView.selected.selected.collect {
        case bf: ItemFigure ⇒
          val oldLoc = bf.getBounds.getLocation
          (bf.tree -> (MPoint(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      val command = TreeCommand(new CopyTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, guiSize) if (positions.contains(v)) ⇒
            ValDef(name, typeName, positions(v), transform(guiSize))
        }
      })
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { modelView.selected.selected collect { case bf: ItemFigure ⇒ bf } foreach { _.moveDeltaFeed(delta) } }
    def abort() {
      modelView.selected.selected collect { case bf: ItemFigure ⇒ bf } foreach { _.moveDeltaFeed(Vector2(0, 0)) }
      exit()
    }
  }
  /// MARQUEE
  object marqueeing extends MovingState {
    def doEnter() {
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
    def move { viewer.moveMarquee(new Rectangle(mouseLocation, initDrag)) }
  }
  // RESIZING
  object resizing extends MovingState {
    var handle: HandleRectangle = _
    def itf = handle.resizeItemFigure

    def enter(initDrag: Point, handle: HandleRectangle) {
      super.enter(initDrag)
      this.handle = handle
    }
    def doEnter {}
    def buttonUp {
      val newBounds = handle.deltaAdd(delta, itf.getBounds);
      val dim = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(15, 15))
      // TODO val comm = new ResizeCommand(itf.resizable, MPoint(newBounds.x,newBounds.y), dim)
      // TODO controller.exec(comm)
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
