package org.zaluum.nide.zge
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.jface.resource.{ ImageRegistry, ImageDescriptor }
import org.eclipse.swt.SWT
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.graphics.{ Image, GC, Font, Point }
import org.eclipse.swt.layout.{ GridLayout, FillLayout }
import org.eclipse.swt.widgets.{ Display, Shell, Composite }
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.icons.Icons
import org.zaluum.nide.model.BoxClass

class ImageFactory(val display: Display, bcp: BoxClassPath) {
  val reg = new ImageRegistry
  reg.put("*", ImageDescriptor.createFromFile(classOf[Icons], "notFound.png"))
  def notFound = reg.get("*")
  def get(resource: String) = {
    Option(reg.get(resource)) orElse {
      val url = bcp.getResource(resource);
      url map { u ⇒
        reg.put(resource, ImageDescriptor.createFromURL(u))
        reg.get(resource)
      }
    }
  }
  def apply(boxClass: Option[BoxClass]): Image = {
    def defaultImage(bc: BoxClass) = bc.className.replace('.', '/') + ".png";
    boxClass map { c ⇒
      get(c.image).orElse { get(defaultImage(c)) }.getOrElse { generateImage(c) } // FIXME dispose generateImage 
    } getOrElse { notFound }
  }
  def generateImage(boxClass: BoxClass): Image = {
    val img = new Image(display, 48, 48);
    val gc = new GC(img)
    val font = new Font(display, "Arial", 6, SWT.NONE);
    gc.setFont(font)
    gc.drawRectangle(0, 0, 47, 47)
    gc.drawText(boxClass.classNameWithoutPackage.getOrElse(boxClass.className), 1, 20);
    gc.dispose
    font.dispose
    img
  }
}
class SelectionManager {
  var selected = Set[Selectable]()
  def apply(f: Selectable) = selected(f)
  def select(f: Selectable) {
    if (!selected(f)) {
      selected += f
      f.showFeedback
    }
  }
  def deselect(f: Selectable) {
    if (selected(f)) {
      f.hideFeedback
      selected -= f
    }
  }
  def toggleSelection(f: Selectable) {
    if (selected(f)) deselect(f)
    else select(f)
  }
  def deselectAll() { selected foreach (deselect(_)) }
  def updateSelection(figs: Set[Selectable], shift: Boolean) {
    if (shift) {
      figs foreach { toggleSelection(_) }
    } else {
      deselectAll()
      figs foreach { select(_) }
    }
  }
}
abstract class ModelViewMapper[M, V <: CanShowUpdate](modelView:AbstractModelView) {
  var viewMap = Map[M, V]()
  def modelSet: Set[M]
  def buildFigure(m: M): V
  def update() {
    val removed = viewMap.keySet -- modelSet
    val added = modelSet -- viewMap.keys
    removed foreach { m ⇒ 
      val f = viewMap(m); 
      f.hide;
      f match {
        case s : Selectable => modelView.selected.deselect(s)
        case _ =>
      }
      viewMap -= m 
    }
    for (m ← added) {
      val f = buildFigure(m)
      viewMap += (m -> f)
      f.show
    }
    viewMap.values foreach { _.update() }
  }
  def values = viewMap.values
  def apply(m: M) = viewMap(m)
  def get(m: M) = viewMap.get(m)
}
abstract class ScrollPopup(mainShell: Shell) {
  var loc: Point = _
  def display = mainShell.getDisplay
  def name: String
  def columns: Int
  def size = new Point(400, 300)
  def populate(content: Composite)
  val popup = new PopupDialog(mainShell, SWT.ON_TOP, true,
    true, true,
    false, false,
    null, name) {
    override def createDialogArea(parent: Composite) = {
      val composite = super.createDialogArea(parent).asInstanceOf[Composite]
      composite.setBackground(display.getSystemColor(SWT.COLOR_BLACK))
      composite.setLayout(new FillLayout)
      val scroll = new ScrolledComposite(composite, SWT.H_SCROLL | SWT.V_SCROLL)
      val content = new Composite(scroll, SWT.NONE);
      scroll.setContent(content);
      {
        val layout = new GridLayout
        layout.numColumns = columns
        layout.verticalSpacing = 10;
        layout.makeColumnsEqualWidth = true;
        content.setLayout(layout)
      }
      populate(content)
      content.setSize(content.computeSize(SWT.DEFAULT, SWT.DEFAULT))
      composite
    }
    override def getDefaultLocation(iniSize: Point) = loc
    override def getDefaultSize() = size
  }
  def show(loc: Point) {
    this.loc = loc
    popup.open;

  }
  def hide() {
    popup.close
  }
}