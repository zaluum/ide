package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.Cursors
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Geometry
import org.zaluum.nide.compiler.LabelDesc
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Param
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Vector2
import draw2dConversions._
import org.zaluum.nide.compiler.MapTransformer

/**
 * Implements basic selecting, marquee and resizing of ItemFigures
 * @author frede
 *
 */
abstract class ItemTool(viewer: ItemViewer) extends LayeredTool(viewer) {
  def selecting: Selecting
  type C = ContainerItem
  state = selecting
  // SELECTING 
  abstract class Selecting extends ToolState {
    var beingSelected: Option[Item] = None
    var initDrag: Point = _
    var initContainer: C = _
    var filterDouble = false
    def doubleClickPF: PartialFunction[Item, String ⇒ MapTransformer] = {
      case e: LiteralFigure ⇒ s ⇒ e.valDef.addOrReplaceParam(Param(Name("literal"), s))
      case l: LabelItem     ⇒ l.valDef.editLabel(false, _)
    }
    override def doubleClick() = itemUnderMouse match {
      case Some(e: TextEditFigure) ⇒
        doubleClickPF.lift(e) foreach { directEditing.enter(e, _) }
      case _ ⇒
    }

    def enter() { state = this }

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
  // SPECIAL move
  trait SpecialMove[A <: Item] {
    self: ToolState with DeltaMove with SingleContainer ⇒
    var fig: A = _
    def enter(initDrag: Point, initContainer: ContainerItem, fig: A) {
      this.fig = fig
      enterMoving(initDrag)
      enterSingle(initContainer)
      state = this
    }
    def clampDelta: Vector2
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { fig.moveFeed(snap(fig.pos + clampDelta)) }
    def abort() {
      fig.moveFeed(fig.pos)
      exit()
    }
  }
  // Direct edit
  object directEditing extends ToolState { // move logic to figure?
    var e: TextEditFigure = null
    var gui = false
    def enter(e: TextEditFigure, command: String ⇒ MapTransformer, gui: Boolean = false) {
      state = this
      this.e = e;
      this.gui = gui
      e.edit({ str ⇒ controller.exec(command(str)) }, exit _)
    }
    def renamePort(p: PortDeclFigure, str: String) {
      println("rename! " + str)
    }

    def exit() { e.hideEdit(); viewer.focus; selecting.enter(); }
    def buttonDown() { exit() }
    def move() {}
    def buttonUp() {}
    def drag() {}
    override def menu() {}
    def abort() { exit() }
  }
  /// MARQUEE
  object marqueeing extends DeltaMove with SingleContainer {
    var absInit = Point(0, 0)
    def enter(p: Point, initContainer: C) {
      import RichFigure._
      enterSingle(initContainer)
      enterMoving(p)
      absInit = initContainer.translateToViewport(p)
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
      val r = new Rectangle(point(currentMouseLocation), point(initDrag))
      val touching = initContainer.shallowItems filter { i ⇒ (r.touches(i.getBounds)) }
      val sel = for (i ← touching; s ← i.selectionSubject) yield s
      viewer.selection.updateSelection(sel.toSet, shift)
      viewer.refresh
      exit()
    }
    override def move {
      viewer.moveMarquee(new Rectangle(point(absMouseLocation), point(absInit)))
    }
  }
  // RESIZING
  val resizing = new Resizing
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

      val newSize = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(24, 24))
      // TODO snap
      itf match {
        case vd: ValDefItem ⇒
          controller.exec(command(newPos, newSize, vd.valDef))
        case _ ⇒ abort()
      }
    }
    def command(newPos: Point, newSize: Dimension, t: Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v: ValDef if (v == t) ⇒
          v.copy(size = Some(newSize), pos = newPos)
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
