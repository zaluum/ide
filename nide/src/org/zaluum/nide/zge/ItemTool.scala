package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.Cursors
import org.zaluum.nide.compiler._
import org.zaluum.nide.eclipse.PaletteEntry

/**
 * Implements basic selecting, marquee and resizing of ItemFigures
 * @author frede
 *
 */
abstract class ItemTool(viewer: ItemViewer) extends LayeredTool(viewer) {
  val gui: Boolean
  def selecting: Selecting
  type C = ContainerItem
  state = selecting

  // SELECTING 
  abstract class Selecting extends ToolState with DeleteState with ClipboardState {
    var beingSelected: Option[Item] = None
    var initDrag: Point = _
    var initContainer: C = _
    var filterDouble = false
    var editFigure: Option[TextEditFigure] = None
    def editLabel(s: String, l: LabelItem): MapTransformer
    def doubleClickPF: PartialFunction[Item, Unit] = {
      case l: LabelItem ⇒ directEditing.enter(l, s ⇒ editLabel(s, l))
      case v: ValDefItem ⇒
        v.openConfigurer(viewer.zproject.classLoader) foreach {
          params ⇒ controller.exec(v.valDef.replaceParams(params))
        }
    }
    override def doubleClick() = itemUnderMouse foreach { doubleClickPF.lift }
    def enter() { state = this }
    def drop() {}
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
    def abort() {}
    def exit() {}

  }
  abstract class Pasting extends ToolState {
    self: SingleContainer ⇒
    var feed: ItemFeedbackFigure = _
    var clipboard: Clipboard = _
    def enter(c: Clipboard, initContainer: ContainerItem) {
      enterSingle(initContainer)
      this.clipboard = c
      state = this
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, 24, 24)); // XXX real clipboard size
      feed.show()
      move()
    }
    def move() { feed.setInnerLocation(point(snap(currentMouseLocation))) }
    def abort { exit() }
    def drag {}
    val gui: Boolean
    def buttonUp = controller.exec(clipboard.pasteCommand(initContainer, absMouseLocation, gui)) // FIXME vector2 -> delta
    def buttonDown() {}
    def exit() {
      feed.hide();
      feed = null;
      selecting.enter()
    }
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
    def move() { fig.moveFeed(fig.pos + clampDelta) }
    def abort() {
      fig.moveFeed(fig.pos)
      exit()
    }
  }
  // Moving label
  trait MovingLabel extends SpecialMove[LabelItem] {
    self: ToolState with DeltaMove with SingleContainer ⇒
    def minDist = 5
    def clampDelta = {
      val pos = fig.pos + delta
      val lblr = fig.getBounds().getCopy()
      lblr.setLocation(point(pos))
      val valFig = fig.container.findFigureOf(fig.valDef).get
      val valr = valFig.getBounds()
      val newx = Snap.snap(lblr.x, lblr.right, valr.x, valr.right, pos.x, minDist)
      val newy = Snap.snap(lblr.y, lblr.bottom, valr.y, valr.bottom, pos.y, minDist)
      Vector2(newx - fig.pos.x, newy - fig.pos.y)
    }
    def lbl = if (gui) fig.valDef.labelGui else fig.valDef.label
    def buttonUp {
      val oldPos = lbl.get.pos
      val newPos = oldPos + clampDelta
      controller.exec(fig.valDef.moveLabel(gui, newPos))
    }
  }
  object movingLabel extends MovingLabel with DeltaMove with SingleContainer
  // Creating
  abstract class Creating extends ToolState {
    var feed: ItemFeedbackFigure = _
    var entry: PaletteEntry = null
    def allowed: Boolean
    def enter(entry: PaletteEntry) {
      state = this
      this.entry = entry
      val size = getSize(entry)
      feed = new ItemFeedbackFigure(viewer)
      feed.setInnerBounds(new Rectangle(0, 0, size.w, size.h));
      feed.show()
    }
    var newVal: ValDef = null
    var initStr: String = ""
    override def next(d: DMap) {
      exit();
      afterCreation.enter(newVal, initStr)
    }
    protected def getSize(entry: PaletteEntry): Dimension
    def move() {
      if (feed == null) throw new Exception
      feed.setInnerLocation(point(snap(currentMouseLocation)))
    }
    def abort() { exit() }
    def drag() {}
    def buttonDown() {}
    protected def newInstanceTemplate(dst: Point, blocks: Int): Option[EditTransformer]
    protected def newInstance(dst: Point): Option[EditTransformer]
    def buttonUp() {
      val dst = snap(currentMouseLocation)
      val command =
        Expressions.templateExpressions.get(entry.className) match {
          case Some(e) ⇒ newInstanceTemplate(dst, e.requiredBlocks)
          case None    ⇒ newInstance(dst)
        }
      command match {
        case Some(c) ⇒ controller.exec(c)
        case None    ⇒ exit()
      }

    }
    def exit() {
      feed.hide();
      feed = null;
      selecting.enter()
    }
  }
  trait SelectionDelegate extends ToolState {
    def buttonDown() { exit(); selecting.buttonDown() }
    def move() { selecting.move() }
    def buttonUp() {}
    def drag() { exit(); selecting.drag() }
    override def menu() { exit(); selecting.menu() }
    override def doubleClick() { exit(); selecting.doubleClick() }
    def abort() { exit(); selecting.abort() }
  }
  object afterCreation extends SelectionDelegate {
    var l: LabelItem = null
    def enter(valDef: ValDef, initialStr: String) {
      state = this
      viewer.findFigureOf(valDef) match {
        case Some(vd) ⇒
          this.l = new LabelItem(vd.container, gui) {
            override def text = initialStr
          }
          l.updateValDef(valDef)
          l.edit(rename _, () ⇒ exit())
        case None ⇒ exit()
      }
    }
    private def rename(str: String) {
      controller.exec(l.valDef.createLabelAndRename(gui, str))
    }
    def exit() {
      if (l != null) {
        l.destroy()
        l = null
      }
      selecting.enter()
    }
  }
  object directEditing extends SelectionDelegate {
    var t: TextEditFigure = null
    def enter(t: TextEditFigure, commandOk: String ⇒ MapTransformer) {
      state = this
      this.t = t
      t.edit(str ⇒ controller.exec(commandOk(str)), exit _)
    }
    def exit() {
      t.destroyEdit()
      t = null
      selecting.enter()
    }
  }
  /// MARQUEE
  object marqueeing extends DeltaMove with SingleContainer {
    var absInit = Point(0, 0)
    def enter(p: Point, initContainer: C) {
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
      viewer.redraw()
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
    var initCoords: Point = _
    def enter(initDrag: Point, initContainer: C, handle: HandleRectangle) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      this.handle = handle
      initCoords = handle.coords(itf.getBounds())
      state = this
    }
    def buttonUp {
      val newBounds = handle.deltaAdd(snapDelta, itf.getBounds);
      val newPos = newBounds.getLocation

      val newSize = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(6, 6))
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
    def snapDelta = snap(initCoords + delta) - initCoords
    def move() {
      itf.resizeDeltaFeed(snapDelta, handle)
    }
    def abort() {
      itf.resizeDeltaFeed(Vector2(0, 0), handle)
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
  }

}
