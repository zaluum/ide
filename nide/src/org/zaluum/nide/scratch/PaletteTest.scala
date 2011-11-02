package org.zaluum.nide.scratch
import org.eclipse.swt.widgets._
import org.eclipse.swt.layout._
import org.eclipse.swt.SWT
import org.eclipse.swt.events.MouseAdapter
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.graphics.Point
import org.zaluum.nide.utils.SWTScala._
import org.eclipse.swt.events.MouseTrackAdapter
import org.eclipse.swt.events.DisposeListener
import org.eclipse.swt.events.DisposeEvent
import org.eclipse.jface.window.Window
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.swt.events.ShellListener
import org.eclipse.swt.events.ShellAdapter
import org.eclipse.swt.events.ShellEvent
import org.eclipse.swt.graphics.GC
import org.zaluum.nide.utils.Utils._
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.events.ControlAdapter
import org.eclipse.swt.events.ControlEvent
import org.zaluum.nide.palette.FirstPalettePopup
object PaletteTest {
  def main(args: Array[String]) {
    val display = new Display()
    val shell = new Shell(display)
    shell.setBounds(10, 10, 500, 500)
    shell.setLayout(new FillLayout)
    shell.setLayout(new FillLayout(SWT.VERTICAL))
    prepare(shell)
    shell.pack(true)
    shell.open()
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) display.sleep();
    }
    display.dispose();
  }
  def prepare(shell: Shell) {
    val c = new Composite(shell, SWT.NONE)
    c.setLayout(new FillLayout)
    val t = new Button(c, SWT.BORDER)
    t.setText("hola")
    val b = new Button(c, SWT.BORDER)
    b.setText("adeu")
    t.addMouseListener(new MouseAdapter {
      override def mouseUp(m: MouseEvent) {
        if (m.button == 3) {
          // val p = new FirstPalettePopup(shell)
          // p.open()
        }
      }
    })
  }
}

