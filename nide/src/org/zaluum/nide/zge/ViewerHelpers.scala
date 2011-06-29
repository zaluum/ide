package org.zaluum.nide.zge

import org.eclipse.jface.resource.LocalResourceManager
import org.eclipse.swt.graphics.Device
import org.eclipse.jface.resource.DeviceResourceDescriptor
import org.eclipse.jface.resource.JFaceResources
import org.eclipse.jface.resource.ResourceManager
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.AbstractBackground
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.Figure
import org.zaluum.nide.eclipse.ClassPath
import org.zaluum.nide.compiler._
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.jface.resource.{ ImageRegistry, ImageDescriptor }
import org.eclipse.swt.SWT
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.graphics.{ Image, GC, Font, Point }
import org.eclipse.swt.layout.{ GridLayout, FillLayout }
import org.eclipse.swt.widgets.{ Display, Shell, Composite }
import org.zaluum.nide.icons.Icons
import org.zaluum.nide.eclipse.ZaluumProject
import org.eclipse.core.resources.IProject


class SelectionManager[A] {
  protected var selected = Set[A]()
  def currentSelected = selected
  override def toString = selected.toString
  def isEmpty = selected.isEmpty
  protected var listeners = Set[() ⇒ Unit]()
  def addListener(a: () ⇒ Unit) { listeners += a }
  def removeListener(a: () ⇒ Unit) { listeners -= a }
  def notifyListeners() { listeners foreach { _() } }
  def refresh(f: PartialFunction[A, A]) {
    selected = selected flatMap { f.lift(_) }
    notifyListeners()
  }
  def apply(t: A) = selected(t)
  def toggleSelection(f: A) {
    if (selected(f)) selected -= f
    else selected += f
    notifyListeners()
  }
  def deselectAll() {
    selected = selected.empty
    notifyListeners()
  }
  def updateSelection(trees: Set[A], shift: Boolean) {
    if (shift) {
      trees foreach { toggleSelection(_) }
    } else {
      selected = selected.empty
      trees foreach { selected += _ }
    }
    notifyListeners()
  }
}

abstract class ScrollPopup(mainShell: Shell) {
  var loc: Point = _
  def display = mainShell.getDisplay
  def name: String
  def columns: Int
  def size = new Point(400, 300)
  def populate(content: Composite, scroll: ScrolledComposite)
  val popup = new PopupDialog(mainShell, SWT.ON_TOP, true,
    true, true,
    false, false,
    null, name) {
    override def createDialogArea(parent: Composite) = {
      val composite = super.createDialogArea(parent).asInstanceOf[Composite]
      composite.setBackground(display.getSystemColor(SWT.COLOR_BLACK))
      composite.setLayout(new FillLayout)
      val scroll = new ScrolledComposite(composite, SWT.V_SCROLL)
      val content = new Composite(scroll, SWT.NONE);
      content.setBackground(ColorConstants.blue)
      scroll.setContent(content);
      populate(content, scroll)
      content.setSize(content.computeSize(SWT.DEFAULT, SWT.DEFAULT))
      composite
    }
    override def getDefaultLocation(iniSize: Point) = loc
    override def getDefaultSize() = size
  }
  def show() {
    this.loc = Display.getCurrent.getCursorLocation
    popup.open;

  }
  def hide() {
    popup.close
  }
}
object DotPainter {
  def dotFill(graphics: Graphics, b: Rectangle, dx: Int, dy: Int) {
    graphics.fillRectangle(b);
    for (i ← 0 to b.width by dx; j ← 0 to b.height by dy) {
      graphics.drawPoint(i, j);
    }
  }
}