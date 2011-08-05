package org.zaluum.nide.zge

import org.eclipse.swt.events.DragDetectEvent
import org.eclipse.swt.events.DragDetectListener
import org.eclipse.swt.events.FocusEvent
import org.eclipse.swt.events.FocusListener
import org.eclipse.swt.events.KeyEvent
import org.eclipse.swt.events.KeyListener
import org.eclipse.swt.events.MenuDetectEvent
import org.eclipse.swt.events.MenuDetectListener
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.events.MouseListener
import org.eclipse.swt.events.MouseMoveListener
import org.eclipse.swt.events.MouseTrackListener
import org.eclipse.swt.events.MouseWheelListener
import org.eclipse.swt.events.TraverseEvent
import org.eclipse.swt.events.TraverseListener
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.Point
object Tool {
  val gridSize = 6
}
abstract class Tool(viewer: Viewer) {
  def viewport = viewer
  def canvas = viewer.canvas
  def controller = viewer.controller

  val listener = new MouseMoveListener() with MouseListener with KeyListener with FocusListener with DragDetectListener with MenuDetectListener with MouseTrackListener with MouseWheelListener with TraverseListener {
    def dragDetected(e: DragDetectEvent) { updateMouse(e); state.drag() }
    def focusGained(e: FocusEvent) {}
    def focusLost(e: FocusEvent) {}
    def keyPressed(e: KeyEvent) = {
      if (e.keyCode == SWT.ESC) handleAbort()
    }
    def keyReleased(e: KeyEvent) {}
    def menuDetected(e: MenuDetectEvent) { state.menu() }
    def mouseScrolled(e: MouseEvent) {}
    def mouseDoubleClick(me: MouseEvent) {
      updateMouse(me);
      if (leftButton(me)) {
        state.doubleClick()
      }
    }
    def mouseDown(me: MouseEvent) {
      updateMouse(me);
      if (leftButton(me)) state.buttonDown()
    }
    def mouseEnter(me: MouseEvent) { updateMouse(me); state.move(); }
    def mouseExit(e: MouseEvent) {}
    def mouseHover(e: MouseEvent) {}
    def mouseMove(me: MouseEvent) { updateMouse(me); state.move() }
    def mouseUp(me: MouseEvent) {
      updateMouse(me);
      if (leftButton(me)) state.buttonUp()
    }
    def keyTraversed(e: TraverseEvent) {}
  }
  canvas.addKeyListener(listener);
  canvas.addDragDetectListener(listener);
  canvas.addFocusListener(listener);
  canvas.addMenuDetectListener(listener)
  canvas.addMouseListener(listener)
  canvas.addMouseMoveListener(listener);
  canvas.addMouseTrackListener(listener);
  canvas.addMouseWheelListener(listener);
  canvas.addTraverseListener(listener);
  def refresh() {
    state.exit()
    state.move() // TODO some kind of refresh?
  }
  trait ToolState {
    def exit()
    def buttonDown()
    def move()
    def buttonUp()
    def drag()
    def menu() {}
    def doubleClick() {}
    def abort()
  }

  var state: ToolState = _
  var stateMask = 0
  var absMouseLocation = Point(0, 0)

  def snap(p: Point): Point = {
      def snap(a: Int) = (math.round(a / Tool.gridSize) * Tool.gridSize).asInstanceOf[Int]
    Point(snap(p.x), snap(p.y))
  }
  var swtMouseLocation = new org.eclipse.swt.graphics.Point(0, 0)
  def updateMouseWithDisplayCoordinates(x: Int, y: Int) {
    swtMouseLocation.x = x
    swtMouseLocation.y = y
    val canvasLocation = canvas.getDisplay.map(null, canvas, swtMouseLocation)
    val absMouse = new org.eclipse.draw2d.geometry.Point(canvasLocation.x, canvasLocation.y)
    viewport.translateFromParent(absMouse);
    absMouseLocation = Point(absMouse.x, absMouse.y)
  }
  def updateMouse(me: MouseEvent) {
    stateMask = me.stateMask
    swtMouseLocation.x = me.x
    swtMouseLocation.y = me.y
    swtMouseLocation = canvas.getDisplay.map(canvas, null, swtMouseLocation)
    val absMouse = new org.eclipse.draw2d.geometry.Point(me.x, me.y)
    viewport.translateFromParent(absMouse);
    absMouseLocation = Point(absMouse.x, absMouse.y)
  }

  def leftButton(me: MouseEvent) = me.button == 1
  def shift = (stateMask & SWT.SHIFT) != 0
  def handleAbort() { state.abort() }
  trait DeleteState {
    def delete()
  }
  trait DropState {
    def drop(s: String)
  }
  def handleDel() {
    state match {
      case d: DeleteState ⇒ d.delete()
      case _              ⇒
    }
  }
  def handleDrop(x: Int, y: Int, s: String) {
    updateMouseWithDisplayCoordinates(x, y)
    state.move()
    state match {
      case d: DropState ⇒ d.drop(s)
      case _            ⇒
    }
  }
  trait ClipboardState {
    def cut()
    def copy()
    def paste()
  }
  def handleCut() = state match {
    case c: ClipboardState ⇒ c.cut
    case _                 ⇒
  }
  def handleCopy() = state match {
    case c: ClipboardState ⇒ c.copy
    case _                 ⇒
  }
  def handlePaste() = state match {
    case c: ClipboardState ⇒ c.paste
    case _                 ⇒
  }
}
