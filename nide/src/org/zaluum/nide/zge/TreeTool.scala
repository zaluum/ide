package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.compiler.{ Point ⇒ MPoint, _ }
import scala.collection.JavaConversions._
import org.zaluum.nide.runtime.LoopBox

class TreeTool(val viewer: TreeViewer) extends ItemTool(viewer) {
  def tree = viewer.tree
  type C = BoxDefContainer
  override lazy val selecting = new TreeSelecting
  class TreeSelecting extends Selecting with DeleteState {
    var port: Option[PortFigure] = None
    var lineSelected: Option[LineFigure] = None
    override def buttonDown {
      super.buttonDown()
      lineSelected = itemOrLineUnderMouse collect { case l: LineFigure ⇒ l }
    }
    override def buttonUp {
      (selected, lineSelected) match {
        case (Some(box), _) ⇒ viewer.selection.updateSelection(Set(box.tree), shift)
        case (None, Some(line)) ⇒ line.con foreach { c ⇒ viewer.selection.updateSelection(Set(c), shift) }
        case (None, None) ⇒ viewer.selection.deselectAll()
      }
      viewer.refresh()
    }
    val portsTrack = new OverTrack[PortFigure] {
      def container = viewer.portsLayer
      def onEnter(p: PortFigure) { port = Some(p); p.showFeedback }
      def onExit(p: PortFigure) { port = None; p.hideFeedback }
    }
    override def move() {
      super.move()
      portsTrack.update()
    }
    override def drag { // TODO inherit item drag
      (handle, selected, port) match {
        case (Some(h), _, _) ⇒ // resize
          resizing.enter(initDrag, initContainer, h)
        case (None, _, Some(port)) ⇒ // connect
          connecting.enter(initContainer, port)
        case (None, Some(fig), _) ⇒ // select and move
          if (!viewer.selection(fig.tree)){
            viewer.selection.updateSelection(Set(fig.tree), shift)
            fig.showFeedback()
          }
          fig match {
            case oPort: OpenPortDeclFigure ⇒ movingOpenPort.enter(initDrag, initContainer, oPort)
            case _ ⇒ moving.enter(initDrag, initContainer)
          }
        case (None, None, None) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    def delete() {
      controller.exec(Delete.deleteSelection(viewer.selectedItems))
    }
    override def menu() {
      itemOrLineUnderMouse match {
        case Some(p: PortDeclFigure) ⇒ new PortDeclPopup(viewer, p.tree).show(swtMouseLocation) // TODO Dispose?
        case Some(p: OpenPortDeclFigure) ⇒ new PortDeclPopup(viewer, p.tree).show(swtMouseLocation)
        case Some(o: OpenBoxFigure) ⇒
        case Some(b: ImageValFigure) ⇒
        case _ ⇒ viewer.palette.show(swtMouseLocation, current)
      }
    }
  }
  abstract class InnerCreating extends ToolState {
    self: SingleContainer ⇒
    var feed: ItemFeedbackFigure = _
    def enter(initContainer: BoxDefContainer) {
      enterSingle(initContainer)
      state = this
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, 48, 48))
      feed.show()
    }
    def move() { feed.setInnerLocation(currentMouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = MPoint(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val sym = b.symbol.asInstanceOf[BoxTypeSymbol]
            val name = Name(sym.freshName("box"))
            val className = Name(sym.freshName("C"))
            val newDef = BoxDef(className, Some(Name(classOf[LoopBox].getName)), None, List(),
              vals = List(),
              ports = List(),
              connections = List())
            val newVal = ValDef(name, className, dst, Some(Dimension(200, 200)), None, None)
            BoxDef(b.name, b.superName, b.image,
              newDef :: transformTrees(b.defs),
              newVal :: transformTrees(b.vals),
              transformTrees(b.ports),
              transformTrees(b.connections))
        }
      }
      controller.exec(tr)
    }
    def buttonDown() {}
    def exit() {
      feed.hide()
      selecting.enter()
    }
  }
  object innercreating extends InnerCreating with SingleContainerAllower with Allower // inherit
  abstract class Creating extends ToolState {
    self: SingleContainer ⇒
    var feed: ItemFeedbackFigure = _
    var tpe: BoxTypeSymbol = _
    def enter(tpe: BoxTypeSymbol, initContainer: BoxDefContainer) {
      enterSingle(initContainer)
      this.tpe = tpe
      state = this
      val img = viewer.imageFactory(tpe.decl);
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      feed.show()
    }
    def move() { feed.setInnerLocation(currentMouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = MPoint(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val name = Name(b.symbol.asInstanceOf[BoxTypeSymbol].freshName("box"))
            BoxDef(b.name, b.superName, b.image,
              transformTrees(b.defs),
              ValDef(name, tpe.name, dst, None, None, None) :: transformTrees(b.vals),
              transformTrees(b.ports),
              transformTrees(b.connections))
        }
      }
      controller.exec(tr)
    }
    def buttonDown() {}
    def exit() {
      feed.hide();
      feed = null;
      selecting.enter()
    }
  }
  // CREATING BOX 
  object creating extends Creating with SingleContainerAllower
  // CREATING PORT
  class CreatingPort extends ToolState {
    self: SingleContainer ⇒
    def enter(dir: PortDir, initContainer: BoxDefContainer) {
      enterSingle(initContainer)
      state = this
      this.dir = dir
      val img = viewer.imageFactory.get(PortDeclFigure.img(dir)).get
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      feed.show()
    }
    var feed: ItemFeedbackFigure = _
    var dir: PortDir = In
    def move() { feed.setInnerLocation(currentMouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      val pos = MPoint(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val tpe = b.symbol.asInstanceOf[BoxTypeSymbol]
            val name = Name(tpe.freshName("port"))
            val p = PortDef(name, Name("D"), dir, pos, MPoint(0, pos.y))
            BoxDef(b.name, b.superName, b.image,
              transformTrees(b.defs),
              transformTrees(b.vals),
              p :: transformTrees(b.ports),
              transformTrees(b.connections))
        }
      }
      controller.exec(tr)
    }
    def buttonDown() {}
    def exit() { feed.hide(); feed = null; selecting.enter() }
  }
  object creatingPort extends CreatingPort with SingleContainerAllower
  // MOVING OPEN PORT
  trait MovingOpenPort {
    self: ToolState with DeltaMove with SingleContainer ⇒
    var fig: OpenPortDeclFigure = _
    def enter(initDrag: Point, initContainer: BoxDefContainer, fig: OpenPortDeclFigure) {
      this.fig = fig
      enterMoving(initDrag)
      enterSingle(initContainer)
      state = this
    }
    def minY = 0
    def maxY = fig.openBox.size.height
    def posY = fig.relPos.y
    def minDelta = minY - posY
    def maxDelta = maxY - posY - fig.size.height
    def clamp(low: Int, i: Int, high: Int) = math.max(low, math.min(i, high))
    def clampDelta = Vector2(0, clamp(minDelta, delta.y, maxDelta))
    def buttonUp {
      val oldPos = fig.tree.extPos
      val newPos = oldPos + clampDelta
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case p: PortDef if (fig.tree == p) ⇒
            p.copy(extPos = newPos)
        }
      }
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { fig.moveDeltaFeed(clampDelta) }
    def abort() {
      fig.moveDeltaFeed(Vector2(0, 0))
      exit()
    }
  }
  object movingOpenPort extends MovingOpenPort with DeltaMove with SingleContainer
  // CONNECT
  trait Connecting extends ToolState {
    self: SingleContainer ⇒
    var dst: Option[PortFigure] = None
    var src: Option[PortFigure] = None
    var painter: ConnectionPainter = _
    val portsTrack = new OverTrack[PortFigure] {
      def onEnter(p: PortFigure) {
        if (p.container == initContainer) {
          dst = Some(p); p.showFeedback
        }
      }
      def onExit(p: PortFigure) { dst = None; p.hideFeedback }
    }
    def enter(initContainer: C, initPort: PortFigure) {
      state = this
      enterSingle(initContainer)
      painter = new ConnectionPainter(initContainer.asInstanceOf[BoxDefContainer])
      src = Some(initPort)

      viewer.setCursor(Cursors.HAND)
      move()
    }
    def doEnter {}
    def buttonUp {
      // execute model command
      if (dst.isDefined) {
        def toRef(pf: PortFigure) = {
          pf.valSym.map { s ⇒ ValRef(s.name) } getOrElse { ThisRef }
        }
        val srcPortName = src.get.sym.name
        val dstPortName = dst.get.sym.name
        val srcRef = toRef(src.get)
        val dstRef = toRef(dst.get)
        if (srcRef != dstRef) { // MORE checks?
          val condef = ConnectionDef(
            PortRef(srcRef, srcPortName, src.get.in),
            PortRef(dstRef, dstPortName, dst.get.in))
          controller.exec(
            new EditTransformer {
              val trans: PartialFunction[Tree, Tree] = {
                case b: BoxDef if (b == initContainer.boxDef) ⇒
                  BoxDef(b.name, b.superName, b.image,
                    transformTrees(b.defs),
                    transformTrees(b.vals),
                    transformTrees(b.ports),
                    condef :: transformTrees(b.connections))
              }
            })
        } else exit()
      } else {
        exit()
      }
    }
    def drag {}
    def buttonDown {}
    def exit() {
      painter.clear
      dst foreach { _.hideFeedback }
      dst = None
      viewer.setCursor(null)
      selecting.enter()
    }
    def move() {
      portsTrack.update()
      val start = src.get.anchor
      val end = dst match {
        case Some(df) ⇒ df.anchor
        case None ⇒ currentMouseLocation
      }
      painter.paintRoute(Route(start, end), false)
    }
    def abort() { exit() }

  }
  object connecting extends Connecting with SingleContainer

}
