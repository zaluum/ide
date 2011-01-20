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
class Palette(viewer: Viewer, mainShell: Shell) extends ScrollPopup(mainShell) {
  def name = "Palette"
  def columns = 4
  def populate(content:Composite) {
    def portDecl(in:Boolean){
        val b = new Button(content, SWT.PUSH)
        val data = new GridData
        data.horizontalAlignment = SWT.CENTER
        b.setLayoutData(data)
        val desc = if (in) "in" else "out"
        b.setToolTipText("Port " + desc)
        val image= viewer.imageFactory.get(PortDeclFigure.img(in)).get
        b.setImage(image)
        b.setSize(48, 48)
        addReaction(b){
          viewer.tool.state.abort()
          viewer.tool.creatingPort.enter(in)
          viewer.canvas.setFocus()
          hide()
        }
      }
      portDecl(in=true)
      portDecl(in=false)
      val classes = viewer.controller.bcp.boxClasses.toBuffer.sortWith(_.className < _.className)
      for (bc <- classes) {
        val b = new Button(content, SWT.PUSH)
        val data = new GridData
        data.horizontalAlignment = SWT.CENTER
        b.setLayoutData(data)
        b.setToolTipText(bc.className)
        val image= viewer.imageFactory(Some(bc))
        b.setImage(image)
        b.setSize(48, 48)
        addReaction(b){
          viewer.tool.state.abort()
          viewer.tool.creating.enter(bc)
          viewer.canvas.setFocus()
          hide()
        }
      }
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