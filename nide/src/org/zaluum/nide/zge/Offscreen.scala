package org.zaluum.nide.zge


 
/*
 * GC example snippet: capture a widget image with a GC
 *
 * For a list of all SWT example snippets see
 * http://www.eclipse.org/swt/snippets/
 */
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Display
import org.eclipse.swt._;
import org.eclipse.swt.events._;
import org.eclipse.swt.graphics._;
import org.eclipse.swt.widgets._;

object Snippet95 {

  def main( args:Array[String]) {
  val display = new Display();
  val shellCapture = new Shell(display);
  val shell = new Shell(display,SWT.TOOL);
  shell.setText("Widget");
  
  val table = new Table(shell, SWT.MULTI);
  table.setLinesVisible(true);
  table.setBounds(10, 10, 100, 100);
  for (i <- 0 to 9) {
    new TableItem(table, SWT.NONE).setText("item" + i);
  }
  val button = new Button(shellCapture, SWT.PUSH);
  button.setText("Capture");
  button.pack();
  button.setLocation(10, 140);
  button.addListener(SWT.Selection, new Listener() {
    def handleEvent(event : Event) {
      val tableSize = table.getSize();
      val bounds = display.getBounds()
      shell.setVisible(true)
      shell.setLocation(bounds.x + bounds.width, 0)
      val gc = new GC(table);
      val image =
        new Image(display, tableSize.x, tableSize.y);
      gc.copyArea(image, 0, 0);
      gc.dispose();
      val popup = new Shell(shell);
      popup.setText("Image");
      popup.addListener(SWT.Close, new Listener() {
        def handleEvent(e : Event) {
          image.dispose();
        }
      });
      
      val canvas = new Canvas(popup, SWT.NONE);
      canvas.setBounds(10, 10, tableSize.x+10, tableSize.y+10);
      canvas.addPaintListener(new PaintListener() {
        def paintControl(e: PaintEvent) {
          e.gc.drawImage(image, 0, 0);
        }
      });
      popup.pack();
      popup.open();
    }
  });
  shell.pack();
  shellCapture.pack()
  shellCapture.open();
  while (!shell.isDisposed()) {
    if (!display.readAndDispatch()) display.sleep();
  }
  display.dispose();
}
}