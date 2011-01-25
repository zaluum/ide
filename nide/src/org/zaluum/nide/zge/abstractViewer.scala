package org.zaluum.nide.zge

import javax.swing.UIManager
import org.eclipse.draw2d.{ FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, FreeformViewport, LightweightSystem, ColorConstants, Figure, IFigure, RectangleFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point }
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Cursor
import org.eclipse.swt.widgets.{ Composite, MessageBox }
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import org.zaluum.nide.compiler.ScannedBoxClassPath
import org.zaluum.nide.model._
import scala.collection.mutable.{ Buffer, Stack }
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import scala.collection.JavaConversions._

abstract class AbstractViewer[M](parent: Composite, val controller: AbstractController[M]) {
  /*SWT*/
  def shell = parent.getShell
  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val connectionsLayer = new FreeformLayer
  val layer = new FreeformLayer
  //layer.setLayoutManager(new FreeformLayout)
  val viewport = new FreeformViewport();
  val innerLayers = new ScalableFreeformLayeredPane()
  val marquee = new RectangleFigure;
  {
    canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)
    layer.setOpaque(true);
    layer.setBackgroundColor(ColorConstants.white)
    innerLayers.add(layer)
    innerLayers.add(portsLayer)
    innerLayers.add(connectionsLayer)
    innerLayers.add(feedbackLayer)
    viewport.setContents(innerLayers);
    canvas.setViewport(viewport)
    marquee.setFill(false)
    marquee.setLineStyle(SWT.LINE_DASH);
    UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
  }
  
  /*DEFS*/
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r: Rectangle) { marquee.setBounds(r) }
  def hideMarquee() { feedbackLayer.remove(marquee) }
  def executeOrNotify(cmd: Command) = {
    if (cmd.canExecute) {
      controller.exec(cmd)
      true
    } else {
      val msg = new MessageBox(shell, SWT.ICON_WARNING | SWT.OK)
      msg.setMessage("Cannot be executed")
      msg.open
      false
    }
  }
  def dispose() {
    canvas.dispose()
  }
  def setCursor(cursor: Cursor) {
    canvas.setCursor(cursor)
  }
  def findDeepAt(container: IFigure, p: Point) = {
    Option(container.findFigureAt(p.x, p.y)) filter (_ != container)
  }
  def findShallowAt(container: IFigure, p: Point) = {
    import scala.collection.JavaConversions._
    container.getChildren.asInstanceOf[java.util.List[IFigure]] find { _.containsPoint(p) };
  }
  def figureAt(p: Point) = findShallowAt(layer, p) map { case (bf: ItemFigure) ⇒ bf }
  def feedbackAt(p: Point) = findDeepAt(feedbackLayer, p)
  def lineAt(p: Point) = findDeepAt(connectionsLayer, p) map { case l: LineFigure ⇒ l }
  def modelView : AbstractModelView[M]
  def tool : AbstractTool[M]
}
abstract class AbstractModelView[M](val viewer:AbstractViewer[M]) {
  val selected = new SelectionManager()
  def deselectAll() {selected.deselectAll}
  def update()
}
abstract class AbstractController[M] {
  val model:M
  var undoStack = Stack[Command]()
  var redoStack = Stack[Command]()
  var mark: Option[Command] = None
  def isDirty = undoStack.elems.headOption != mark
  def markSaved() { mark = undoStack.elems.headOption }
  def updateViewers
  def abortTools()
  def exec(c: Command) {
    if (c.canExecute) {
      abortTools()
      c.act()
      undoStack.push(c)
      redoStack.clear
      updateViewers
      notifyListeners
    }
  }
  def canUndo = !undoStack.isEmpty
  def canRedo = !redoStack.isEmpty

  def undo() {
    if (!undoStack.isEmpty) {
      abortTools()
      val c = undoStack.pop
      redoStack.push(c)
      c.undo()
      updateViewers
      notifyListeners
    }
  }
  def redo() {
    if (!redoStack.isEmpty) {
      abortTools()
      val c = redoStack.pop
      undoStack.push(c)
      // update tools
      c.redo()
      updateViewers
      notifyListeners
    }
  }
  var listeners = Set[() ⇒ Unit]()
  def addListener(action: () ⇒ Unit) {
    listeners += action
  }
  def removeListener(action: () ⇒ Unit) {
    listeners -= action
  }
  def notifyListeners() {
    listeners foreach { _() }
  }
}
