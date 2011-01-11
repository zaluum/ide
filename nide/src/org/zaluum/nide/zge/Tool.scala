package org.zaluum.nide.zge

import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.FreeformViewport
import org.eclipse.draw2d.geometry.Point
import org.eclipse.swt.SWT
import org.eclipse.swt.events._
import org.eclipse.swt.widgets.Canvas
import scala.reflect.Manifest._

abstract class Tool(viewer: Viewer) {
  def viewport = viewer.viewport
  def canvas = viewer.canvas
  def controller = viewer.controller
  def modelView = viewer.modelView
  def figureUnderMouse = viewer.figureAt(mouseLocation)
  def connectionUnderMouse = viewer.connectionAt(mouseLocation)
  def feedbackUnderMouse = viewer.feedbackAt(mouseLocation)
  val listener = new MouseMoveListener() with MouseListener with KeyListener with FocusListener with DragDetectListener with MenuDetectListener with MouseTrackListener with MouseWheelListener with TraverseListener {
    def dragDetected(e: DragDetectEvent) { updateMouse(e); state.drag() }
    def focusGained(e: FocusEvent) { println("gain" + e) }
    def focusLost(e: FocusEvent) { println("lost" + e) }
    def keyPressed(e: KeyEvent) = { 
      if (e.keyCode == SWT.ESC) handleAbort()
    }
    def keyReleased(e: KeyEvent) {}
    def menuDetected(e: MenuDetectEvent) { println("menu" + e) }
    def mouseScrolled(e: MouseEvent) { println("scroll" + e) }
    def mouseDoubleClick(me: MouseEvent) { updateMouse(me); }
    def mouseDown(me: MouseEvent) {
      updateMouse(me);
      if (leftButton(me)) {
        down = true
        state.buttonDown()
      }
    }
    def mouseEnter(e: MouseEvent) {}
    def mouseExit(e: MouseEvent) {}
    def mouseHover(e: MouseEvent) {} //println("hover" + e)}
    def mouseMove(me: MouseEvent) { updateMouse(me); state.move() }
    def mouseUp(me: MouseEvent) {
      updateMouse(me);
      if (leftButton(me)) {
        down = false
        state.buttonUp()
      }
    }
    def keyTraversed(e: TraverseEvent) { println("traverse" + e) }
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

  trait ToolState {
    def enter() { state = this }
    def exit()
    def buttonDown() 
    def move() 
    def buttonUp() 
    def drag() 
    def abort() 
  }

  trait MovingState extends ToolState {
    var initDrag: Point = _
    var d = (0,0)
    def enter(initDrag:Point) {
      enter()
      this.initDrag = initDrag
      doEnter()
    }
    def doEnter()
    def move() { d = delta; doMove()}
    def doMove() 
    def delta = {
      val now = mouseLocation
      (now.x - initDrag.x, now.y - initDrag.y)
    }
  }
  var down = false
  var state: ToolState = _
  var stateMask = 0
  var mouseLocation = new Point
  def updateMouse(me: MouseEvent) {
    stateMask = me.stateMask
    mouseLocation.x = me.x
    mouseLocation.y = me.y
    viewport.translateFromParent(mouseLocation);
  }
  def leftButton(me: MouseEvent) = me.button == 1
  def shift = (stateMask & SWT.SHIFT) != 0
  def handleAbort() { state.abort() }
  // Over track
  abstract class OverTrack[F<:Figure](container : IFigure)(implicit m:Manifest[F] ) {
    var last : Option[F] = None
    def filterManifest[F](o:Option[AnyRef]) = {
      o match { 
        case  Some(s) => 
          if (singleType(s) <:< m)
            Some(s.asInstanceOf[F])
          else
            None
        case None => None
      }
    }
    def update() {
      val under : Option[F]= filterManifest(viewer.findDeepAt(container,mouseLocation))
      if (under==last) return;
      last foreach { f => onExit(f); last = None }
      under foreach { f => onEnter(f); last = Some(f)}
    }
    def onEnter(f:F)
    def onExit(f:F)
  }

}
