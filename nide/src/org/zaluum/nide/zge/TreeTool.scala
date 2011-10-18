package org.zaluum.nide.zge

import scala.annotation.tailrec
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Cursors
import org.eclipse.draw2d.Label
import org.eclipse.draw2d.MarginBorder
import org.eclipse.draw2d.RectangleFigure
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Expressions
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.PortDef
import org.zaluum.nide.compiler.PortDir
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.Template
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Vector2
import draw2dConversions.point
import org.zaluum.nide.compiler.MapTransformer
import org.zaluum.nide.compiler.BoxExprType
import org.zaluum.nide.eclipse.PaletteEntry
import org.zaluum.nide.eclipse.Palette
import org.zaluum.nide.compiler.Param

class TreeTool(val viewer: TreeViewer) extends ItemTool(viewer) with ConnectionsTool {
  val gui = false
  def tree = viewer.tree
  def zproject = viewer.controller.zproject
  val connectionLineDistance = 3
  val gridSize = 1
  object selecting extends Selecting with DeleteState with ClipboardState with DropState with LineBlinker {
    var port: Option[PortFigure] = None
    def editLabel(s: String, l: LabelItem) =
      l.valDef.editLabel(false, s)
    def buttonUp {
      unblinkLine()
      if (filterDouble) { filterDouble = false; return }
        def selectItem(i: Item) {
          viewer.selection.updateSelection(i.selectionSubject.toSet, shift)
          viewer.refresh()
          i.selectionSubject foreach { controller.blink(_, viewer) }
        }
      (beingSelected, port) match {
        case (Some(b: Button), _)             ⇒ actButton(b)
        case (Some(o: OpenPortDeclFigure), _) ⇒ selectItem(o)
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
        case (Some(i: Item), None) ⇒ selectItem(i)
        case (None, _) ⇒
          viewer.selection.deselectAll()
          viewer.refresh()
        case _ ⇒
      }
    }
    val treeDoubleClickPFs: PartialFunction[Item, Unit] = {
      case e: PortDeclFigure ⇒ directEditing.enter(e, e.tree.renamePort(_, None))
      case e: LiteralFigure  ⇒ directEditing.enter(e, s ⇒ e.valDef.addOrReplaceParam(Param(Name("literal"), s)))
    }
    override def doubleClickPF = treeDoubleClickPFs orElse super.doubleClickPF
    def actButton(b: Button) = {
      val template = b.openBox.template
      val i = b.openBox.templateSym.nextBlockIndex
      val command = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case t: Template if t == template ⇒
            t.copy(blocks = transformTrees(t.blocks),
              ports = transformTrees(t.ports),
              currentBlock = Some(i.toString))
        }
      }
      controller.exec(command)
      viewer.selection.deselectAll()
      viewer.refresh()
    }
    val portsTrack = new PortTrack {
      override def onEnter(p: PortFigure) { super.onEnter(p); port = Some(p); viewer.setCursor(Cursors.CROSS) }
      override def onExit(p: PortFigure) { super.onExit(p); port = None }
    }
    override def move() {
      super.move()
      portsTrack.update()
      (portsTrack.current, itemUnderMouse) match {
        case (_, Some(l: OpenPortDeclFigure)) ⇒
          unblinkLine();
          viewer.setCursor(Cursors.ARROW)
        case (_, Some(l: LineItem)) if (l.l.distance(currentMouseLocation) > connectionLineDistance) ⇒
          blinkLine(l);
          viewer.setCursor(Cursors.UPARROW)
        case (None, Some(item)) ⇒
          unblinkLine();
          viewer.setCursor(Cursors.ARROW)
        case (Some(_), _) ⇒
          unblinkLine();
          viewer.setCursor(Cursors.UPARROW)
        case (None, None) ⇒
          unblinkLine();
          viewer.setCursor(Cursors.CROSS)
          portsTrack.current match {
            case Some(_) ⇒
            case None    ⇒
          }
      }
    }
    def drag {
      portsTrack.hideTip()
      unblinkLine()
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
              case label: LabelItem          ⇒ movingLabel.enter(initDrag, initContainer, label)
              case _                         ⇒ moving.enter(initDrag, initContainer)
            }
          }
        case (None, _) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    def drop(a: AnyRef) {
      a match {
        case e: PaletteEntry ⇒
          e match {
            case Palette.InEntry    ⇒ creatingPort.enter(In, current)
            case Palette.OutEntry   ⇒ creatingPort.enter(Out, current)
            case Palette.ShiftEntry ⇒ creatingPort.enter(Shift, current)
            case _                  ⇒ creating.enter(e)
          }
        case _ ⇒
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

  }
  // PASTING
  object pasting extends Pasting with SingleContainerAllower {
    val gui = false
  }
  // CREATING
  abstract class TreeCreating extends Creating {
    self: ContainerHighlighter ⇒
    def allowed = true
    override def enter(entry: PaletteEntry) {
      super.enter(entry)
      enterHighlight(current)
    }
    protected def getSize(entry: PaletteEntry) = {
      val (img, desc) = zproject.imageFactory.image48(entry.className);
      val d = Dimension(img.getBounds().width, img.getBounds.height)
      zproject.imageFactory.destroy(desc)
      d
    }
    protected def newInstance(dst: Point) = {
      Some(new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: Block if b == current.block ⇒
            initStr = entry.className.classNameWithoutPackage.firstLowerCase
            val name = Name(b.sym.freshName(initStr))
            newVal = if (entry.isExpression) {
              ValDef.emptyValDef(name, entry.className, dst)
            } else if (entry.static)
              ValDef.emptyValStaticInvokeExpr(name, dst, entry.className.str, entry.methodUID.getOrElse(""))
            else
              ValDef.emptyValDefBoxExpr(
                name, dst, entry.className.str,
                method = entry.methodUID,
                fields = entry.fields)
            b.copy(
              valDefs = newVal :: transformTrees(b.valDefs),
              connections = transformTrees(b.connections),
              parameters = transformTrees(b.parameters),
              junctions = transformTrees(b.junctions))
        }
      })
    }
    protected def newInstanceTemplate(dst: Point, requiredBlocks: Int) = {
      Some(new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: Block if b == current.block ⇒
            val sym = b.sym
            val name = Name(sym.freshName(entry.className.classNameWithoutPackage.firstLowerCase))
            val template = Template.emptyTemplate(requiredBlocks)
            val newVal = ValDef(name, entry.className, dst, Some(Dimension(200, 200)), List(), None, None, Some(template))
            b.copy(
              valDefs = newVal :: transformTrees(b.valDefs),
              parameters = transformTrees(b.parameters),
              connections = transformTrees(b.connections),
              junctions = transformTrees(b.junctions))
        }
      })
    }
  }
  // CREATING BOX 
  object creating extends TreeCreating with ContainerHighlighter
  // CREATING PORT
  class CreatingPort extends ToolState {
    self: SingleContainer ⇒
    def enter(dir: PortDir, initContainer: ContainerItem) {
      enterSingle(initContainer)
      state = this
      this.dir = dir
      val (img, desc) = zproject.imageFactory.portImg(dir)
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      zproject.imageFactory.destroy(desc)
      feed.show()
    }
    var feed: ItemFeedbackFigure = _
    var dir: PortDir = In
    def move() { feed.setInnerLocation(point(snap(currentMouseLocation))) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      val pos = snap(currentMouseLocation)
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case te: Template if te == initContainer.template ⇒
            val name = Name(initContainer.symbol.freshName("port"))
            val p = PortDef(name, Name("double"), dir, pos, Point(0, pos.y))
            te.copy(
              ports = p :: transformTrees(te.ports),
              blocks = transformTrees(te.blocks))
        }
      }
      controller.exec(tr)
    }
    def buttonDown() {}
    def exit() { feed.hide(); feed = null; selecting.enter() }
  }
  object creatingPort extends CreatingPort with SingleContainerAllower

  // MOVING OPEN PORT
  trait MovingOpenPort extends SpecialMove[OpenPortDeclFigure] {
    self: ToolState with DeltaMove with SingleContainer ⇒
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
  }
  object movingOpenPort extends MovingOpenPort with DeltaMove with SingleContainer
  // HighLighter
  trait ContainerHighlighter extends ToolState {
    var last: ContainerItem = null
    def enterHighlight(init: ContainerItem) {
      last = init
      init.highLight(true)
    }
    override abstract def move() {
      if (current != last && last != null)
        last.highLight(false)
      last = current
      last.highLight(true)
      super.move()
    }
    override abstract def exit() {
      if (last != null) last.highLight(false)
      super.exit()
    }
  }

  class PortTrack extends OverTrack[PortFigure] {
    class TooltipLabel extends RectangleFigure {
      val l = new Label
      l.setBorder(new MarginBorder(4, 4, 4, 4));
      l.setFont(viewer.display.getSystemFont)
      setBackgroundColor(ColorConstants.tooltipBackground)
      setForegroundColor(ColorConstants.tooltipForeground)
      l.setForegroundColor(ColorConstants.tooltipForeground)
      add(l)
      def setText(s: String) { l.setText(s) }
      override def getPreferredSize(x: Int, y: Int) = l.getPreferredSize(x, y)
      override def setSize(x: Int, y: Int) = { l.setSize(x, y); super.setSize(x, y) }
    }
    lazy val tooltip = new TooltipLabel
    def showTip(pf: PortFigure) {
      try {
        val abs = pf.anchor.getCopy
        val name = pf.ps.helperName.getOrElse(pf.ps.name).str
        val pi = pf.ps.pi
        val text = name + " : " + pi.tpe.map { _.name.str }.getOrElse("<Error>")
        tooltip.setText(text)
        viewer.feedbackLayer.add(tooltip)
      } catch {
        case e ⇒ e.printStackTrace()
      }
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
