package org.zaluum.nide.zge

import org.eclipse.swt.graphics.Image
import SWTScala._
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{ Shell, Composite, Button }
import org.zaluum.nide.compiler.ScannedBoxClassPath

object Palette {
  val w = 400
  val h = 300
}
class Palette(viewer: Viewer, mainShell: Shell, bcp: ScannedBoxClassPath) extends ScrollPopup(mainShell) {
  def name = "Palette"
  def columns = 4
  def populate(content: Composite) {
    def portDecl(in: Boolean) {
      val b = new Button(content, SWT.PUSH)
      val data = new GridData
      data.horizontalAlignment = SWT.CENTER
      b.setLayoutData(data)
      val desc = if (in) "in" else "out"
      b.setToolTipText("Port " + desc)
      val image = viewer.imageFactory.get(PortDeclFigure.img(in)).get
      b.setImage(image)
      b.setSize(48, 48)
      addReaction(b) {
        viewer.tool.state.abort()
        // TODO viewer.tool.creatingPort.enter(in)
        viewer.canvas.setFocus()
        hide()
      }
    }
    portDecl(in = true)
    portDecl(in = false)
    def createButton(name:String, image:Image) = {
      val b = new Button(content, SWT.PUSH)
      val data = new GridData
      data.horizontalAlignment = SWT.CENTER
      b.setLayoutData(data)
      b.setToolTipText(name)
      b.setImage(image)
      b.setSize(48, 48)
      b
    }
    val innerb = createButton("INNER",viewer.imageFactory .notFound)
    addReaction(innerb) {
      viewer.tool.state.abort()
      // TODO viewer.tool .innercreating.enter()
      viewer.canvas.setFocus()
      hide()
    }
    val classes = bcp.boxClasses.toBuffer.sortWith(_.className.toString < _.className.toString)
    for (bc ← classes) {
     /* TODO val b = createButton(bc.className.toString,viewer.imageFactory(Some(bc))) 
      addReaction(b) {
        viewer.tool.state.abort()
        viewer.tool.creating.enter(bc)
        viewer.canvas.setFocus()
        hide()
      }*/
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