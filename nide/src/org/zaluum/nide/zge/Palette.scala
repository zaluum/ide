package org.zaluum.nide.zge

import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.runtime.LoopBox
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.PortDir
import org.eclipse.swt.graphics.Point
import SWTScala._
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{ Shell, Composite, Button }
import org.zaluum.nide.compiler.{ BoxTypeSymbol, In, Out }
object Palette {
  val w = 400
  val h = 300
}
class Palette(viewer: TreeViewer, mainShell: Shell, proj: ZaluumProject) extends ScrollPopup(mainShell) {
  def name = "Palette"
  def columns = 4
  var container : ContainerItem  = _
  def show(p:Point, container:ContainerItem) {
    this.container = container
    show(p)
  }
  def populate(content: Composite) {
    def portDecl(dir : PortDir, desc:String) {
      val b = new Button(content, SWT.PUSH)
      val data = new GridData
      data.horizontalAlignment = SWT.CENTER
      b.setLayoutData(data)
      b.setToolTipText("Port " + desc)
      val image = viewer.imageFactory.load(PortDeclFigure.img(dir)).get
      b.setImage(image)
      b.setSize(48, 48)
      addOnDispose(b) {image.dispose()}
      addReaction(b) {
        viewer.tool.state.abort()
        viewer.tool.creatingPort.enter(dir,container)
        viewer.canvas.setFocus()
        hide()
      }
    }
    portDecl(In,"Input port")
    portDecl(Out, "Output port")
    if (container.isInstanceOf[OpenBoxFigure]) 
      portDecl(Shift, "Shift port")
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
    val innerb = createButton("Loop", viewer.imageFactory.notFound)
    addReaction(innerb) {
      viewer.tool.state.abort()
      viewer.tool.innercreating.enter(container, Name(classOf[LoopBox].getName))
      viewer.canvas.setFocus()
      hide()
    }
    val names = proj.index.toBuffer.sortWith(_.toString < _.toString)
    for (name ← names) {
      val img = viewer.imageFactory(name)
      val b = createButton(name.str, img)
      addOnDispose(b){img.dispose}
      addReaction(b) {
        viewer.tool.state.abort()
        viewer.tool.creating.enter(name,container)
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