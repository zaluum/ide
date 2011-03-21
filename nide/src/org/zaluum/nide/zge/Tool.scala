package org.zaluum.nide.zge

import org.eclipse.ui.IViewSite
import org.eclipse.ui.PlatformUI
import scala.annotation.tailrec
import org.eclipse.draw2d.{ Figure, IFigure }
import org.zaluum.nide.compiler.Point
import org.eclipse.swt.SWT
import org.eclipse.swt.events._

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
      if (e.keyCode == SWT.DEL) handleDel()
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
    state.abort()
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
  var absMouseLocation = Point(0,0)
  var swtMouseLocation = new org.eclipse.swt.graphics.Point(0, 0)
  def updateMouse(me: MouseEvent) {
    stateMask = me.stateMask
    swtMouseLocation.x = me.x
    swtMouseLocation.y = me.y
    swtMouseLocation = canvas.getDisplay.map(canvas, null, swtMouseLocation)
    val absMouse = new org.eclipse.draw2d.geometry.Point(me.x, me.y)
    viewport.translateFromParent(absMouse);
    absMouseLocation = Point(absMouse.x,absMouse.y)
  }
  
  def leftButton(me: MouseEvent) = me.button == 1
  def shift = (stateMask & SWT.SHIFT) != 0
  def handleAbort() { state.abort() }
  trait DeleteState {
    def delete()
  }
  def handleDel() { state match {
    case d:DeleteState => d.delete()
    case _ =>
    }
  }
  // Over track

}
