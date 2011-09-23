package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Graphics
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Control
import net.miginfocom.swt.MigLayout
import org.eclipse.jface.viewers.DialogCellEditor
import org.eclipse.swt.widgets.FontDialog
import org.eclipse.swt.graphics.FontData
import org.eclipse.ui.views.properties.PropertyDescriptor
import org.eclipse.jface.viewers.LabelProvider
import org.zaluum.nide.compiler.FontValueType

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
abstract class Popup(mainShell: Shell, name: String) extends PopupDialog(mainShell, PopupDialog.INFOPOPUP_SHELLSTYLE, true,
  true, true,
  false, false,
  null, name) {
  var loc: Point = _
  def display = mainShell.getDisplay
  def size = new Point(450, 100)
  def populate(content: Composite)
  var initialFocus: Control = _
  override def createDialogArea(parent: Composite) = {
    val composite = super.createDialogArea(parent).asInstanceOf[Composite]
    composite.setLayout(new MigLayout(
      "",
      "[][grow][]",
      "[]"))
    populate(composite)
    composite
  }
  override def getDefaultLocation(iniSize: Point) = loc
  override def getFocusControl() = if (initialFocus == null) super.getFocusControl() else initialFocus
  override def open() = {
    this.loc = Display.getCurrent.getCursorLocation
    super.open()
  }
}
abstract class ScrollPopup(mainShell: Shell) {
  var loc: Point = _
  def display = mainShell.getDisplay
  def name: String
  def size = new Point(450, 100)
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

class FontDataPropertyDescriptor(id: AnyRef, displayName: String) extends PropertyDescriptor(id, displayName) {
  setLabelProvider(new LabelProvider() {
    override def getText(element: AnyRef) = element match {
      case f: FontData ⇒ FontValueType.fontToSwingStr(f)
      case _           ⇒ ""
    }
  })
  override protected def createPropertyEditor(parent: Composite) = {
      def validator = getValidator
    new DialogCellEditor(parent) {
      setValidator(validator)
      override protected def openDialogBox(cell: Control) = {
        val dialog = new FontDialog(cell.getShell)
        val v = getValue
        v match {
          case fd: FontData ⇒ dialog.setFontList(Array(fd))
          case _            ⇒
        }
        dialog.open() match {
          case fd: FontData ⇒ fd
          case _            ⇒ v
        }
      }
    }
  }
}