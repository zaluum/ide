package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.Cursors
import org.zaluum.nide.compiler._
import javax.swing.JButton
import javax.swing.JComponent
import java.awt.{ Graphics ⇒ AG }
import java.awt.image.BufferedImage
import org.eclipse.draw2d.{ Figure, Graphics }
import org.eclipse.swt.widgets.{ Composite, Display, Shell, Listener, Event }
import org.eclipse.swt.SWT
import scala.collection.mutable.Buffer

class GuiTool(viewer: GuiViewer) extends ItemTool(viewer) {
  override val resizing = new Resizing {
    override def command(newPos: Point, newSize: Dimension, t: Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize, params) if (v == t) ⇒
          ValDef(name, typeName, pos, size, Some(newPos), Some(newSize), params)
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
        case Some(s: Item) ⇒ viewer.selection.updateSelection(s.selectionSubject.toSet, shift)
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
      case (true, true) ⇒ viewer.setCursor(Cursors.SIZESE)
      case (false, true) ⇒ viewer.setCursor(Cursors.SIZES)
      case (true, false) ⇒ viewer.setCursor(Cursors.SIZEE)
      case _ ⇒ viewer.setCursor(Cursors.ARROW)
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
          moving.enter(initDrag, initContainer)
        case (None, None, _) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
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
    def calcMin: Dimension = {
      val bottomRights = viewer.layer.getChildren.collect { case i: IFigure ⇒ i.getBounds.getBottomRight }
      val minRect = new Rectangle
      bottomRights.foldLeft(Dimension(0, 0))((acc, p) ⇒ Dimension(max(acc.w, p.x), max(acc.h, p.y)))
    }
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
              BoxDef(b.name, b.superName, guiSize = Some(newSize), b.image,
                transformTrees(b.defs),
                transformTrees(b.vals),
                transformTrees(b.ports),
                transformTrees(b.connections),
                transformTrees(b.junctions))
          }
        }
        controller.exec(command)
      }else {
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
      val positions = viewer.selectedItems.collect {
        case item: TreeItem ⇒
          val oldLoc = item.getBounds.getLocation
          (item.tree.asInstanceOf[ValDef] -> (Point(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize, params) if (positions.contains(v)) ⇒
            ValDef(name, typeName, pos, size, Some(positions(v)), guiSize, params)
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
    def currentMouseLocation = GuiTool.this.currentMouseLocation
  }
  object moving extends Moving with DeltaMove
}
