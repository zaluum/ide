package org.zaluum.nide.zge

import org.zaluum.nide.model.Box
import javax.swing.JButton
import javax.swing.JComponent
import java.awt.{ Graphics ⇒ AG }
import java.awt.image.BufferedImage
import org.eclipse.draw2d.{ Figure, Graphics }
import org.eclipse.swt.widgets.{ Composite, Display, Shell, Listener, Event }
import org.eclipse.swt.SWT
import org.zaluum.nide.model.{ Resizable, Dimension, Positionable, Point, Model, Command }
import org.zaluum.nide.compiler.BoxClassPath
import scala.collection.mutable.Buffer

class SwingFigure(val viewer: GUIViewer, val box: Box, val component: JComponent) extends Figure with ResizableItemFigure {
  lazy val feed = new ResizeItemFeedbackFigure(this)
  def positionable = box.guiPos.get // TODO better way? 
  def resizable = box.guiPos.get
  def size = box.guiPos.get.size
  setOpaque(true)
  override def paintFigure(g: Graphics) {
    val rect = getClientArea()
    component.setBounds(0, 0, rect.width, rect.height);
    component.doLayout
    val aimage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB)
    val ag = aimage.createGraphics
    component.paint(ag)
    val imageData = SWTUtils.convertAWTImageToSWT(aimage)
    val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(), imageData)
    g.drawImage(image, rect.x, rect.y)
    ag.dispose();
    image.dispose()
  }
}
class GUIModelView(viewer: GUIViewer, val model: Model, val bcp: BoxClassPath) extends AbstractModelView(viewer) {
  object widgetMapper extends ModelViewMapper[Box, SwingFigure](this) {
    def guiCreator(box: Box) = bcp.find(box.className) filter { _.visual } flatMap { _.guiCreator }
    def modelSet = {
      model.boxes filter { guiCreator(_).isDefined }
    }
    def buildFigure(guiBox: Box) = {
      val component = guiCreator(guiBox) map { _() } getOrElse { new JButton("Not found") }
      if (!guiBox.guiPos.isDefined) {
        guiBox.guiPos = Some(new Resizable { // TODO command?
          var pos = Point(0, 0)
          var size = Dimension(50, 50)
        })
      }
      // TODO catch exceptions 
      new SwingFigure(viewer, guiBox, component)
    }
  }
  def shell = viewer.shell

  def update() {
    widgetMapper.update()
    val s = shell.getSize
    if (s.x != model.guiSize.w && s.y!=model.guiSize.h)
      shell.setSize(model.guiSize.w, model.guiSize.h)
  }
}
object ExampleGUI {
  def simpleModel = {
    val m = new GUIModel
    // val button = new JButton("hola")
    // m.widgets += new Widget(button, Point(50,50), Dimension(60,60))
    m
  }
}
class GUIModel
class GUIViewer(override val shell: Shell, controller: Controller) extends AbstractViewer(shell, controller) {
  /*TOOLS*/
  var tool = new GUITool(this)
  /*MODEL*/
  lazy val modelView = new GUIModelView(this, controller.model, controller.bcp)
  def model = controller.model
  shell.addListener(SWT.Resize, new Listener {
    def handleEvent(e: Event) {
      val b = shell.getSize
      val newd = Dimension(b.x, b.y)
      val oldd = model.guiSize
      controller.exec(new Command() {
        def undo() { model.guiSize = oldd }
        def redo() { model.guiSize = newd }
        def canExecute = newd!=oldd
      })
    }
  })
}
class GUITool(val viewer: GUIViewer) extends AbstractTool(viewer)
