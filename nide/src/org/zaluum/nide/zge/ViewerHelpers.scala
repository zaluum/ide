package org.zaluum.nide.zge

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

class ImageFactory(val display: Display, zp: ZaluumProject) {
  def notFound = ImageDescriptor.createFromFile(classOf[Icons], "notFound.png").createImage()
  def load(resource: String) = {
    val url = zp.getResource(resource);
    url map { u ⇒
      ImageDescriptor.createFromURL(u).createImage()
    }
  }
  def apply(name : Name) : Image = {
    imageFor(None,name);
  }
  def apply(tpe: Type): Image = {
    tpe match {
      case b: BoxTypeSymbol ⇒ imageFor(b.image, b.name)
      case _ ⇒ notFound
    }
  }
  private def imageFor(image: Option[String], name: Name) = {
    def defaultImage(name: Name) = name.toRelativePath + ".png";
    def fallbackImage(name: Name) = load(defaultImage(name)).getOrElse { generateImage(name.classNameWithoutPackage) }
    image.flatMap { load(_) }.getOrElse(fallbackImage(name))
  }
  def apply(typeTree: Tree): Image = {
    typeTree match {
      case b: BoxDef ⇒ imageFor(b.image, b.name)
      case _ ⇒ notFound
    }
  }
  private def generateImage(txt: String): Image = {
    val img = new Image(display, 48, 48);
    val gc = new GC(img)
    val font = new Font(display, "Arial", 6, SWT.NONE);
    gc.setFont(font)
    gc.drawRectangle(0, 0, 47, 47)
    gc.drawText(txt, 1, 20);
    gc.dispose
    font.dispose
    img
  }
}
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
  def populate(content: Composite,scroll:ScrolledComposite)
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
      populate(content,scroll)
      content.setSize(content.computeSize(SWT.DEFAULT,SWT.DEFAULT))
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
  def dotFill(graphics: Graphics, b: Rectangle) {
    graphics.fillRectangle(b);
    for (i ← 0 to b.width by 15; j ← 0 to b.height by 15) {
      graphics.drawPoint(i, j);
    }
  }
}