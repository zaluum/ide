package org.zaluum.nide.zge

import org.eclipse.swt.graphics.Point
import SWTScala._
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{ Shell, Composite, Button }
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.newcompiler.{ BoxTypeSymbol, In, Out }
object Palette {
  val w = 400
  val h = 300
}
class Palette(viewer: TreeViewer, mainShell: Shell, bcp: EclipseBoxClasspath) extends ScrollPopup(mainShell) {
  def name = "Palette"
  def columns = 4
  var container : BoxDefContainer = _
  def show(p:Point, container:BoxDefContainer) {
    this.container = container
    show(p)
  }
  def populate(content: Composite) {
    def portDecl(in: Boolean) {
      val b = new Button(content, SWT.PUSH)
      val data = new GridData
      data.horizontalAlignment = SWT.CENTER
      b.setLayoutData(data)
      val desc = if (in) "in" else "out"
      val dir = if (in) In else Out
      b.setToolTipText("Port " + desc)
      val image = viewer.imageFactory.get(PortDeclFigure.img(dir)).get
      b.setImage(image)
      b.setSize(48, 48)
      addReaction(b) {
        viewer.tool.state.abort()
        viewer.tool.creatingPort.enter(dir,container)
        viewer.canvas.setFocus()
        hide()
      }
    }
    portDecl(in = true)
    portDecl(in = false)
    def createButton(name: String, image: Image) = {
      val b = new Button(content, SWT.PUSH)
      val data = new GridData
      data.horizontalAlignment = SWT.CENTER
      b.setLayoutData(data)
      b.setToolTipText(name)
      b.setImage(image)
      b.setSize(48, 48)
      b
    }
    val innerb = createButton("INNER", viewer.imageFactory.notFound)
    addReaction(innerb) {
      viewer.tool.state.abort()
      viewer.tool.innercreating.enter(container)
      viewer.canvas.setFocus()
      hide()
    }
    val classes = bcp.boxes.toBuffer.sortWith(_.name.toString < _.name.toString)
    for (tpe ← classes) {
      val bc = tpe.asInstanceOf[BoxTypeSymbol]
      val b = createButton(bc.name.str, viewer.imageFactory(bc))
      addReaction(b) {
        viewer.tool.state.abort()
        viewer.tool.creating.enter(bc,container)
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