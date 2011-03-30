package org.zaluum.nide.zge

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
  type C = Container
  override val resizing = new Resizing {
    override def command(newPos: Point, newSize: Dimension, t: Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize, params) if (v == t) ⇒
          ValDef(name, typeName, pos, size, Some(newPos), Some(newSize), params)
      }
    }
  }
  object selecting extends Selecting  {
    def buttonUp {
      beingSelected match {
        case Some(s: Item) ⇒ viewer.selection.updateSelection(s.selectionSubject.toSet, shift)
        case None ⇒ viewer.selection.deselectAll()
      }
      viewer.refresh()
    }
    def drag {
      (handleTrack.current, beingSelected) match {
        case (Some(h), _) ⇒ // resize
          resizing.enter(initDrag, initContainer, h)
        case (None, Some(fig)) ⇒ // select and move
          if (fig.selectionSubject.isDefined) {
            if (!viewer.selection(fig.selectionSubject.get)) {
              viewer.selection.updateSelection(fig.selectionSubject.toSet, shift)
              fig.showFeedback()
            }
          }
        // FIXME moving.enter(initDrag, initContainer)
        case (None, None) ⇒ marqueeing.enter(initDrag, initContainer) // marquee
      }
    }
  }
  trait Moving extends ToolState {
    self: DeltaMove  ⇒
    def enter(initDrag: Point, initContainer: C) {
      enterMoving(initDrag)
      state = this
    }
    def buttonUp {
      val positions = viewer.selectedItems.collect { case item : TreeItem ⇒
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
