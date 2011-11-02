package org.zaluum.nide.palette
import org.eclipse.swt.widgets._
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.events.MouseAdapter
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.graphics.GC

class CustomExpand(parent: Composite, text: String) extends Composite(parent, SWT.NONE) {
  val lay = new GridLayout(1, false)
  lay.verticalSpacing = 2
  lay.marginHeight = 0
  setLayout(lay)
  val l = new Label(this, SWT.NONE)
  l.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false))
  l.addMouseListener(new MouseAdapter() {
    override def mouseUp(e: MouseEvent) {
      setOpen(!isOpen)
    }
  })
  val c = new Composite(parent, SWT.NONE)
  val gd = new GridData(SWT.FILL, SWT.TOP, true, false)
  c.setLayoutData(gd)
  var t = text
  private var _isOpen = true

  setOpen(true)
  def isOpen = _isOpen
  def setText(t: String) {
    this.t = t
    updateLabel()
  }
  private def updateLabel() {
    l.setText((if (isOpen) "V " else "> ") + t)
  }
  def setOpen(b: Boolean) {
    _isOpen = b
    gd.exclude = !b
    c.setVisible(b)
    updateLabel()
    parent.layout(true)
  }

  /*def drawChevron(gc: GC, x: Int, y: Int) {
    var polyline1, polyline2: Array[Int] = null;
    if (isOpen) {
      var px = x + 4 + 5;
      var py = y + 4 + 7;
      polyline1 = Array(
        px, py, px + 1, py, px + 1, py - 1, px + 2, py - 1, px + 2, py - 2, px + 3, py - 2, px + 3, py - 3,
        px + 3, py - 2, px + 4, py - 2, px + 4, py - 1, px + 5, py - 1, px + 5, py, px + 6, py);
      py += 4;
      polyline2 = Array(
        px, py, px + 1, py, px + 1, py - 1, px + 2, py - 1, px + 2, py - 2, px + 3, py - 2, px + 3, py - 3,
        px + 3, py - 2, px + 4, py - 2, px + 4, py - 1, px + 5, py - 1, px + 5, py, px + 6, py);
    } else {
      var px = x + 4 + 5;
      var py = y + 4 + 4;
      polyline1 = Array(
        px, py, px + 1, py, px + 1, py + 1, px + 2, py + 1, px + 2, py + 2, px + 3, py + 2, px + 3, py + 3,
        px + 3, py + 2, px + 4, py + 2, px + 4, py + 1, px + 5, py + 1, px + 5, py, px + 6, py);
      py += 4;
      polyline2 = Array(
        px, py, px + 1, py, px + 1, py + 1, px + 2, py + 1, px + 2, py + 2, px + 3, py + 2, px + 3, py + 3,
        px + 3, py + 2, px + 4, py + 2, px + 4, py + 1, px + 5, py + 1, px + 5, py, px + 6, py);
    }
    gc.setForeground(getDisplay.getSystemColor(SWT.COLOR_TITLE_FOREGROUND));
    gc.drawPolyline(polyline1);
    gc.drawPolyline(polyline2);
  }*/

  override protected def checkSubclass() {
  }
}