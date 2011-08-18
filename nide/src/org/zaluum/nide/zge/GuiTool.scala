package org.zaluum.nide.zge

import scala.collection.JavaConversions.asScalaBuffer
import scala.math.max
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.Cursors
import org.eclipse.draw2d.IFigure
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Vector2
import org.zaluum.nide.zge.dialogs.ValDefPopup
import draw2dConversions._
class GuiTool(viewer: GuiViewer) extends ItemTool(viewer) {
  def calcMin: Dimension = {
    val bottomRights = viewer.layer.getChildren.collect { case i: IFigure ⇒ rpoint(i.getBounds.getBottomRight) }
    maxPoint(bottomRights)
  }
  def maxPoint(points: Iterable[Point]) = points.foldLeft(Dimension(0, 0))((acc, p) ⇒ Dimension(max(acc.w, p.x), max(acc.h, p.y)))
  override val resizing = new Resizing {
    override def command(newPos: Point, newSize: Dimension, t: Tree) = {
      val newDim = Dimension(newPos.x + newSize.w, newPos.y + newSize.h)
      new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == viewer.boxDef ⇒
            b.copy(guiSize = Some(viewer.backRect.getSize.ensureMin(newDim)),
              template = transform(b.template))
          case v: ValDef if (v == t) ⇒
            v.copy(guiPos = Some(newPos), guiSize = Some(newSize))
        }
      }
    }
  }
  object selecting extends Selecting {
    var border = (false, false)
    override def buttonDown {
      border = borderDistance
      super.buttonDown
    }
    def buttonUp {
      beingSelected match {
        case Some(s: Item) ⇒
          viewer.selection.updateSelection(s.selectionSubject.toSet, shift)
          s.selectionSubject foreach { controller.blink(_, viewer) }
        case None ⇒ viewer.selection.deselectAll()
      }
      viewer.refresh()
    }
    val borderSensivity = 5
    def borderDistance = {
      val br = viewer.backRect.getBounds.getBottomRight
      (math.abs(currentMouseLocation.x - br.x) < borderSensivity, math.abs(currentMouseLocation.y - br.y) < borderSensivity)
    }
    override def move {
      borderDistance match {
        case (true, true)  ⇒ viewer.setCursor(Cursors.SIZESE)
        case (false, true) ⇒ viewer.setCursor(Cursors.SIZES)
        case (true, false) ⇒ viewer.setCursor(Cursors.SIZEE)
        case _             ⇒ viewer.setCursor(Cursors.ARROW)
      }
      super.move
    }
    def drag {
      (handleTrack.current, beingSelected, border) match {
        case (_, _, a) if a != (false, false) ⇒
          resizingGui.enter(initDrag, a)
        case (Some(h), _, _) ⇒ // resize
          resizing.enter(initDrag, initContainer, h)
        case (None, Some(fig), _) ⇒ // select and move
          if (fig.selectionSubject.isDefined) {
            if (!viewer.selection(fig.selectionSubject.get)) {
              viewer.selection.updateSelection(fig.selectionSubject.toSet, shift)
              fig.showFeedback()
            }
          }
          fig match {
            case s: SwingFigure ⇒ moving.enter(initDrag, initContainer)
            case l: LabelItem   ⇒ movingLabel.enter(initDrag, initContainer, l)
          }
        case (None, None, _) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
    override def menu() = {
      itemUnderMouse match {
        case Some(l: LabelItem)   ⇒ new ValDefPopup(viewer, l, true).open()
        case Some(s: SwingFigure) ⇒ new ValDefPopup(viewer, s, true).open()
        case _                    ⇒
      }
    }
  }
  trait ResizingGui extends ToolState {
    self: DeltaMove ⇒
    import draw2dConversions._
    import scala.collection.JavaConversions._
    import math.max
    var mode = (false, false)
    var initSize = Dimension(0, 0)
    var minSize = Dimension(0, 0)
    def currentMouseLocation = GuiTool.this.currentMouseLocation

    def enter(initDrag: Point, mode: (Boolean, Boolean)) {
      enterMoving(initDrag)
      initSize = viewer.backRect.getSize
      minSize = calcMin
      this.mode = mode
      state = this
    }
    def filterDelta = Vector2(if (mode._1) delta.x else 0, if (mode._2) delta.y else 0)
    def newSize = (initSize + filterDelta).ensureMin(minSize).ensureMin(Dimension(10, 10))
    def move {
      viewer.backRect.setSize(dimension(newSize))
    }
    def buttonUp {
      if (newSize != initSize) {
        val command = new EditTransformer {
          val trans: PartialFunction[Tree, Tree] = {
            case b: BoxDef if (b == viewer.boxDef) ⇒
              b.copy(
                guiSize = Some(newSize),
                template = transform(b.template))
          }
        }
        controller.exec(command)
      } else {
        exit()
      }
    }
    def buttonDown {}
    def drag() {}
    def abort() {
      viewer.backRect.setSize(dimension(initSize))
      exit()
    }
    def exit() { selecting.enter() }
  }
  object resizingGui extends ResizingGui with DeltaMove

  // MOVING
  trait Moving extends ToolState {
    self: DeltaMove ⇒
    def enter(initDrag: Point, initContainer: C) {
      enterMoving(initDrag)
      state = this
    }
    def buttonUp {
      val positionsTuple = viewer.selectedItems.collect {
        case item: SwingFigure ⇒
          val itemPos = snap(item.pos + delta)
          val itemBottom = itemPos + item.size.toVector
          (item.valDef, itemPos, itemBottom)
      }
      val positions = positionsTuple.map { case (v, i, _) ⇒ v -> i } toMap
      val newDim = maxPoint(positionsTuple.map { case (_, _, b) ⇒ b }).ensureMin(viewer.backRect.getSize)
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if (b == viewer.boxDef) ⇒
            b.copy (template=transform(b.template), guiSize = Some(newDim))
          case v: ValDef if (positions.contains(v)) ⇒
            v.copy(guiPos = Some(positions(v)))
        }
      }
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { viewer.selectedItems foreach { f ⇒ f.moveFeed(snap(f.pos + delta)) } }
    def abort() {
      viewer.selectedItems foreach { f ⇒ f.moveFeed(f.pos) }
      exit()
    }
    def currentMouseLocation = GuiTool.this.currentMouseLocation
  }
  object moving extends Moving with DeltaMove
  // Moving label
  trait MovingLabel extends SpecialMove[LabelItem] { // TODO inherit treetool movingLabel
    self: ToolState with DeltaMove with SingleContainer ⇒
    def clampDelta = delta
    def buttonUp {
      val oldPos = fig.valDef.labelGui.get.pos
      val newPos = oldPos + clampDelta
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v: ValDef if (fig.valDef == v) ⇒
            v.copy(labelGui = Some(v.labelGui.get.copy(pos = newPos)))
        }
      }
      controller.exec(command)
    }
  }
  object movingLabel extends MovingLabel with DeltaMove with SingleContainer

}
