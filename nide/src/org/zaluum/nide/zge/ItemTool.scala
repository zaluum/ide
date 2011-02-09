package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ Cursors, Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import org.zaluum.nide.newcompiler.{ Transformer, Tree, ValDef, CopyTransformer, PortDef }
import scala.collection.JavaConversions._
import scala.reflect.Manifest._

abstract class LayeredTool(viewer: ItemViewer) extends Tool(viewer) {
  def figureUnderMouse = current.figureAt(currentMouseLocation)
  def lineUnderMouse = current.lineAt(currentMouseLocation)
  def feedbackUnderMouse = current.feedbackAt(currentMouseLocation)
  def currentMouseLocation = translate(current, absMouseLocation)
  def current = FiguresHelper.findDeepAt(viewer.layer, absMouseLocation) {
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
    def filterManifest[F](o: Option[AnyRef]) = {
      o match {
        case Some(s) ⇒
          if (singleType(s) <:< m)
            Some(s.asInstanceOf[F])
          else
            None
        case None ⇒ None
      }
    }
    def update() {
      val under: Option[F] = filterManifest(viewer.findDeepAt(container, currentMouseLocation))
      if (under == last) return ;
      last foreach { f ⇒ onExit(f); last = None }
      under foreach { f ⇒ onEnter(f); last = Some(f) }
    }
    def onEnter(f: F)
    def onExit(f: F)
  }
}
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
    var selected: Option[ItemFigure] = None
    var lineSelected: Option[LineFigure] = None
    var handle: Option[HandleRectangle] = None
    var port: Option[PortFigure] = None
    var initDrag: Point = _
    var initContainer: Layers = _
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
          moving.enter(initDrag, initContainer)
        case (None, None, None) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    def connect(port: PortFigure) {}
    def abort {}
    def exit {}
  }
  // MOVE
  object moving extends MovingState {
    def doEnter {}
    def doButtonUp {
      val positions = viewer.selected.selected.collect {
        case bf: ItemFigure ⇒
          val oldLoc = bf.getBounds.getLocation
          (bf.tree -> (MPoint(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      val command = TreeCommand(new CopyTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, guiSize) if (positions.contains(v)) ⇒
            ValDef(name, typeName, positions(v), transform(guiSize))
          case p: PortDef if (positions.contains(p)) ⇒
            p.copy(inPos = positions(p))
        }
      })
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    override def move() { super.move(); viewer.selected.selected collect { case bf: ItemFigure ⇒ bf } foreach { _.moveDeltaFeed(delta) } }
    override def abort() {
      super.abort()
      viewer.selected.selected collect { case bf: ItemFigure ⇒ bf } foreach { _.moveDeltaFeed(Vector2(0, 0)) }
      exit()
    }
  }
  trait AllowerState extends ToolState {
    private var storedCursor: Option[Cursor] = None
    def allowed: Boolean
    override def move() {
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
    }
    override def buttonUp() = if (allowed) doButtonUp else abort()
    override def abort() = storedCursor.foreach { viewer.setCursor(_) }
    def doButtonUp()
  }
  trait SingleContainerState extends AllowerState {
    var initContainer: Layers = _
    def enter(initContainer: Layers) {
      this.initContainer = initContainer
    }
    def currentMouseLocation = translate(initContainer,absMouseLocation)
    def allowed = initContainer eq current
  }
  trait MovingState extends SingleContainerState {
    var initDrag: Point = _
    def enter(initDrag: Point, initContainer: Layers) {
      super.enter(initContainer)
      state = this
      this.initContainer = initContainer
      this.initDrag = initDrag
      doEnter()
    }
    def doEnter()
    def delta = {
      val now = currentMouseLocation
      Vector2(now.x - initDrag.x, now.y - initDrag.y)
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
    override def abort() { super.abort(); exit() }
    def buttonDown {}
    def drag {}
    def doButtonUp {
      viewer.hideMarquee()
      exit()
    }
    override def move { super.move; viewer.moveMarquee(new Rectangle(currentMouseLocation, initDrag)) } // FIXME
  }
  // RESIZING
  object resizing extends MovingState {
    var handle: HandleRectangle = _
    def itf = handle.resizeItemFigure

    def enter(initDrag: Point, initContainer: Layers, handle: HandleRectangle) {
      super.enter(initDrag, initContainer)
      this.handle = handle
    }
    def doEnter {}
    def doButtonUp {
      val newBounds = handle.deltaAdd(delta, itf.getBounds);
      val dim = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(15, 15))
      // TODO val comm = new ResizeCommand(itf.resizable, MPoint(newBounds.x,newBounds.y), dim)
      // TODO controller.exec(comm)
    }
    override def move() { super.move(); itf.resizeDeltaFeed(delta, handle) }
    override def abort() {
      super.abort()
      itf.resizeDeltaFeed(Vector2(0, 0), handle)
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
  }

}
