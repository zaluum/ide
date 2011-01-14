package org.zaluum.nide.zge

import org.eclipse.swt.layout.GridData
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.events.PaintEvent
import org.eclipse.swt.widgets.Event
import org.eclipse.swt.widgets.Listener
import org.eclipse.swt.events.PaintListener
import org.eclipse.swt.events.{ MouseTrackAdapter, MouseEvent }
import org.eclipse.swt.graphics.Rectangle
import org.eclipse.swt.graphics.Region
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.widgets.{ Shell, ExpandBar, Composite, Button, ExpandItem }
import org.eclipse.swt.SWT
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.layout.{ FillLayout, GridLayout };
import SWTScala._
object Palette {
  val w = 400
  val h = 300
}
class Palette(viewer: Viewer, mainShell: Shell) {
  var loc: Point = _
  def display = mainShell.getDisplay
  val popup = new PopupDialog(mainShell, SWT.ON_TOP, true,
    true, true,
    false, false,
    null, "palette") {
    override def createDialogArea(parent: Composite) = {
      
      val composite = super.createDialogArea(parent).asInstanceOf[Composite]
      composite.setBackground(display.getSystemColor(SWT.COLOR_BLACK))
      composite.setLayout(new FillLayout)
      val scroll = new ScrolledComposite(composite, SWT.H_SCROLL | SWT.V_SCROLL)
      val content = new Composite(scroll, SWT.NONE);
      scroll.setContent(content);
      {
        val layout = new GridLayout
        layout.numColumns=4
        layout.verticalSpacing = 10;
        content.setLayout(layout)
      }
      val b = new Button(content, SWT.PUSH)
      b.setText("Crear")
      addReaction(b){
        viewer.tool.state.abort()
        viewer.tool.creating.enter(viewer.controller.bcp.find("graystone.zaluum.SumBox").get)
        viewer.canvas.setFocus()
        hide()
      }
      content.setSize(content.computeSize(SWT.DEFAULT,SWT.DEFAULT))
      composite
    }
    override def getDefaultLocation(iniSize: Point) = loc
    override def getDefaultSize() = new Point(400, 300)
  }
  /* */
  // shell.layout()
  def show(loc: Point) {
    this.loc = loc
    popup.open;

  }
  def hide() {
    popup.close
  }
  def dispose() {
    //popup.dispose()
  }
  
}
/*
val bar = new ExpandBar (shell, SWT.V_SCROLL);
  val composite = new Composite (bar, SWT.NONE);
  val layout = new GridLayout ();
  layout.marginLeft = 10
  layout.marginTop = 10
  layout.marginRight = 10 
  layout.marginBottom = 10
  layout.verticalSpacing = 10;
  composite.setLayout(layout);
  var button = new Button (composite, SWT.PUSH);
  button.setText("SWT.PUSH");
  button = new Button (composite, SWT.RADIO);
  button.setText("SWT.RADIO");
  button = new Button (composite, SWT.CHECK);
  button.setText("SWT.CHECK");
  button = new Button (composite, SWT.TOGGLE);
  button.setText("SWT.TOGGLE");
  val item0 = new ExpandItem (bar, SWT.NONE, 0);
  item0.setText("What is your favorite button");
  item0.setHeight(composite.computeSize(SWT.DEFAULT, SWT.DEFAULT).y);
  item0.setControl(composite);*/ 