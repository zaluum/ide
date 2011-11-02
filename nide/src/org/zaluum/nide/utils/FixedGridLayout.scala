package org.zaluum.nide.utils
import org.eclipse.swt.widgets._
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.SWT

class FixedGridLayout(val cols: Int, val w: Int, val h: Int, val space: Int) extends Layout {
  def computeSize(composite: Composite, wHint: Int, hHint: Int, flushCache: Boolean): Point = {
    val l = composite.getChildren().length
    val rows = if (l == 0) 0 else ((l - 1) / cols) + 1
    new Point(totalW, rows * (h + space) + space)
  }
  def totalW = (w + space) * 4
  def layout(c: Composite, b: Boolean) {
    val cw = c.getClientArea().width
    val offsetX = if (cw > totalW) (cw - totalW) / 2 else 0
    for ((child, i) ← c.getChildren.zipWithIndex) {
      val xpos = (i % cols)
      val ypos = (i / cols)
      child.setBounds(offsetX + xpos * (w + space), space + ypos * (h + space), w, h)
    }
  }
}