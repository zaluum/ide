package org.zaluum.nide.zge

import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Label
import org.zaluum.nide.compiler.NoSymbol
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.ToolTip
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint, Rectangle, Dimension ⇒ EDimension }
import org.zaluum.nide.compiler.{ _ }
import scala.collection.JavaConversions._
import org.zaluum.basic.LoopBox

class TreeTool(val viewer: TreeViewer) extends ItemTool(viewer) with ConnectionsTool {
  def tree = viewer.tree
  val connectionLineDistance = 2
  object selecting extends Selecting with DeleteState with ClipboardState {
    var port: Option[PortFigure] = None
    override def doubleClick() {
      itemUnderMouse match {
        case Some(e: DirectValFigure) ⇒ directEditing.enter(e)
        case _ ⇒
      }
    }
    def buttonUp {
      if (filterDouble) { filterDouble = false; return }
      (beingSelected, port) match {
        case (_, Some(port)) ⇒ // connect
          portsTrack.hideTip()
          connecting.enter(port.container, port, currentMouseLocation)
        case (Some(line: LineItem), None) ⇒
          if (line.l.distance(currentMouseLocation) <= connectionLineDistance) {
            viewer.selection.updateSelection(line.selectionSubject.toSet, shift)
            viewer.refresh()
          } else {
            connecting.enter(line.container, line, currentMouseLocation)
          }
        case (Some(box: Item), None) ⇒
          viewer.selection.updateSelection(box.selectionSubject.toSet, shift)
          viewer.refresh()
        case (None, _) ⇒
          viewer.selection.deselectAll()
          viewer.refresh()
        case _ ⇒
      }

    }
    val portsTrack = new PortTrack {
      override def onEnter(p: PortFigure) { super.onEnter(p); port = Some(p); viewer.setCursor(Cursors.CROSS) }
      override def onExit(p: PortFigure) { super.onExit(p); port = None }
    }
    override def move() {
      super.move()
      portsTrack.update()
      (portsTrack.current, itemUnderMouse) match {
        case (_, Some(l: LineItem)) if (l.l.distance(currentMouseLocation) > connectionLineDistance) ⇒
          viewer.setCursor(Cursors.UPARROW)
        case (None, Some(item)) ⇒
          viewer.setCursor(Cursors.ARROW)
        case (Some(_), _) ⇒
          viewer.setCursor(Cursors.UPARROW)
        case (None, None) ⇒
          viewer.setCursor(Cursors.CROSS)
          portsTrack.current match {
            case Some(_) ⇒
            case None ⇒
          }
      }
    }
    def drag {
      portsTrack.hideTip()
      (handleTrack.current, beingSelected) match {
        case (Some(h), _) ⇒ // resize
          resizing.enter(initDrag, initContainer, h)
        case (None, Some(fig: Item)) ⇒ // select and move
          val s = fig.selectionSubject
          if (!s.isEmpty) {
            if (!viewer.selection(s.get))
              viewer.selection.updateSelection(s.toSet, false)
            fig.showFeedback()
            fig match {
              case oPort: OpenPortDeclFigure ⇒ movingOpenPort.enter(initDrag, initContainer, oPort)
              case _ ⇒ moving.enter(initDrag, initContainer)
            }
          }
        case (None, _) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    def delete() {
      controller.exec(Delete.deleteSelection(viewer.selectedItems, viewer.graphOf))
    }
    def cut() {
      viewer.updateClipboard
      delete
    }
    def copy() = viewer.updateClipboard
    def paste() = viewer.getClipboard foreach { c ⇒ pasting.enter(c, current) }
    override def menu() {
      itemUnderMouse match {
        case Some(p: PortDeclFigure) ⇒ new PortDeclPopup(viewer, p.tree).show(swtMouseLocation) // TODO Dispose?
        case Some(p: OpenPortDeclFigure) ⇒ new PortDeclPopup(viewer, p.tree).show(swtMouseLocation)
        case Some(o: OpenBoxFigure) ⇒
        case Some(b: ValFigure) ⇒ new ConstructorMenu(viewer.shell, controller, b.sym).open;
        case _ ⇒ viewer.palette.show(swtMouseLocation, current)
      }
    }
  }
  abstract class InnerCreating extends ToolState {
    self: SingleContainer ⇒
    var feed: ItemFeedbackFigure = _
    var superName: Name = null
    def enter(initContainer: ContainerItem, superName: Name) {
      enterSingle(initContainer)
      this.superName = superName
      state = this
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, 48, 48))
      feed.show()
    }
    def move() { feed.setInnerLocation(point(currentMouseLocation)) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = Point(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val sym = b.symbol.asInstanceOf[BoxTypeSymbol]
            val name = Name(sym.freshName("box"))
            val className = Name(sym.freshName("C"))
            val newDef = BoxDef(className, Some(superName), guiSize = None, image = None, List(),
              vals = List(),
              ports = List(),
              connections = List(),
              junctions = List())
            val newVal = ValDef(name, className, dst, Some(Dimension(200, 200)), None, None, List(),List(),List())
            BoxDef(b.name, b.superName, guiSize = b.guiSize, b.image,
              newDef :: transformTrees(b.defs),
              newVal :: transformTrees(b.vals),
              transformTrees(b.ports),
              transformTrees(b.connections),
              transformTrees(b.junctions))
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
  // PASTING
  abstract class Pasting extends ToolState {
    self: SingleContainer ⇒
    var feed: ItemFeedbackFigure = _
    var clipboard: Clipboard = _
    def enter(c: Clipboard, initContainer: ContainerItem) {
      enterSingle(initContainer)
      this.clipboard = c
      state = this
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, 48, 48)); // XXX real clipboard size
      feed.show()
      move()
    }
    def move() { feed.setInnerLocation(point(currentMouseLocation)) }
    def abort { exit() }
    def drag {}
    def buttonUp = controller.exec(clipboard.pasteCommand(initContainer, currentMouseLocation))
    def buttonDown() {}
    def exit() {
      feed.hide();
      feed = null;
      selecting.enter()
    }
  }
  object pasting extends Pasting with SingleContainerAllower
  // CREATING
  abstract class Creating extends ToolState {
    self: SingleContainer ⇒
    var feed: ItemFeedbackFigure = _
    var tpeName: Name = _
    def enter(tpename: Name, initContainer: ContainerItem) {
      enterSingle(initContainer)
      this.tpeName = tpename
      state = this
      val img = viewer.imageFactory(tpeName);
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      img.dispose()
      feed.show()
    }
    def move() { feed.setInnerLocation(point(currentMouseLocation)) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = Point(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            //val params = tpe.params.map { p ⇒ Param(p.name, p.default) }.toList
            val name = Name(b.symbol.asInstanceOf[BoxTypeSymbol].freshName("box"))
            BoxDef(b.name, b.superName, guiSize = b.guiSize, b.image,
              transformTrees(b.defs),
              ValDef(name, tpeName, dst, None, None, None, List(),List(),List()) :: transformTrees(b.vals),
              transformTrees(b.ports),
              transformTrees(b.connections),
              transformTrees(b.junctions))
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
    def enter(dir: PortDir, initContainer: ContainerItem) {
      enterSingle(initContainer)
      state = this
      this.dir = dir
      val img = viewer.imageFactory.load(PortDeclFigure.img(dir)).get
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      img.dispose()
      feed.show()
    }
    var feed: ItemFeedbackFigure = _
    var dir: PortDir = In
    def move() { feed.setInnerLocation(point(currentMouseLocation)) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      val pos = Point(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val tpe = b.symbol.asInstanceOf[BoxTypeSymbol]
            val name = Name(tpe.freshName("port"))
            val p = PortDef(name, Name("double"), dir, pos, Point(0, pos.y))
            BoxDef(b.name, b.superName, guiSize = b.guiSize, b.image,
              transformTrees(b.defs),
              transformTrees(b.vals),
              p :: transformTrees(b.ports),
              transformTrees(b.connections),
              transformTrees(b.junctions))
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
    def enter(initDrag: Point, initContainer: ContainerItem, fig: OpenPortDeclFigure) {
      this.fig = fig
      enterMoving(initDrag)
      enterSingle(initContainer)
      state = this
    }
    def minY = 0
    def maxY = fig.openBox.size.h
    def posY = fig.relPos.y
    def minDelta = minY - posY
    def maxDelta = maxY - posY - fig.size.h
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
  // Direct edit
  object directEditing extends ToolState {
    var e: DirectValFigure = null
    def enter(e: DirectValFigure) {
      state = this
      this.e = e;
      e.edit(execute(_), exit _)
    }
    def editParam(valDef: ValDef, key: Name, value: String) {
      val c = valDef.params find { p ⇒ p.asInstanceOf[Param].key == key } match {
        case Some(par: Param) if (par.value != value) ⇒
          controller.exec(
            new EditTransformer() {
              val trans: PartialFunction[Tree, Tree] = {
                case p: Param if p == e.param ⇒ Param(p.key, value)
              }
            })
        case None ⇒
          controller.exec(
            new EditTransformer() {
              val trans: PartialFunction[Tree, Tree] = {
                case v: ValDef ⇒ v.copy(params = Param(key, value) :: transformTrees(v.params))
              }
            })
        case _ ⇒ // TODO exit?
      }
    }
    def execute(s: String) {
      if (e != null) {
        editParam(e.valDef,Name("param"), s)
      }
    }
    def exit() { e.hideEdit(); viewer.focus; selecting.enter(); }
    def buttonDown() { exit() }
    def move() {}
    def buttonUp() {}
    def drag() {}
    override def menu() {}
    def abort() { exit() }
  }
  class PortTrack extends OverTrack[PortFigure] {
    class TooltipLabel extends RectangleFigure {
      val l = new Label
      l.setFont(viewer.display.getSystemFont)
      setBackgroundColor(ColorConstants.tooltipBackground)
      add(l)
      def setText(s: String) { l.setText(s) }
      override def getPreferredSize(x: Int, y: Int) = l.getPreferredSize(x, y)
      override def setSize(x: Int, y: Int) = { l.setSize(x, y); super.setSize(x, y) }
    }
    lazy val tooltip = new TooltipLabel
    def showTip(p: PortFigure) {
      val abs = p.anchor.getCopy
      tooltip.setText(p.sym.name.str + " : " + p.sym.tpe.name.str)
      viewer.feedbackLayer.add(tooltip)

    }
    override def update {
      super.update
      tooltip.setLocation(draw2dConversions.point(absMouseLocation + Vector2(15, 15)))
      tooltip.setSize(tooltip.getPreferredSize())
    }
    def hideTip() {
      if (viewer.feedbackLayer.getChildren.contains(tooltip))
        viewer.feedbackLayer.remove(tooltip)
    }
    override def onEnter(p: PortFigure) { super.onEnter(p); p.hover = true; showTip(p) }
    override def onExit(p: PortFigure) { super.onExit(p); p.hover = false; hideTip }
  }
}
