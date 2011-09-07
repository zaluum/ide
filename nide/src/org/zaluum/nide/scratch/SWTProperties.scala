package org.zaluum.nide.scratch
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.ColorDialog
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.SWT
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.events.SelectionEvent

object SWTProperties {
  def main(args: Array[String]) {
    println("hola")
    val display = new Display()
    val shell = new Shell(display)
    shell.setBounds(10, 10, 500, 500)
    shell.setLayout(new FillLayout)
    shell.open()
    val button = new Button(shell, SWT.PUSH)
    button.setText("hola")
    shell.pack()
    button.addSelectionListener(new SelectionListener() {
      def widgetSelected(e: SelectionEvent) {
        val b = new ColorDialog(shell)
        val result = b.open();
        println(result);
      }
      def widgetDefaultSelected(e: SelectionEvent) {}
    })
    while (!shell.isDisposed()) {
      if (!display.readAndDispatch()) display.sleep();
    }
    display.dispose();
  }
}