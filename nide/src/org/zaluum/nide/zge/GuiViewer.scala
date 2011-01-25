package org.zaluum.nide.zge

import javax.swing.JButton
import javax.swing.JComponent
import java.awt.{ Graphics ⇒ AG }
import java.awt.image.BufferedImage
import org.eclipse.draw2d.{ Figure, Graphics }
import org.eclipse.swt.widgets.{ Composite, Display }
import org.zaluum.nide.model.{ Resizable, Dimension, Positionable, Point }
import scala.collection.mutable.Buffer

class Widget(var component: JComponent, var pos: Point, var size: Dimension) extends Positionable with Resizable

class SwingFigure(val viewer: GUIViewer, val widget: Widget) extends Figure with ResizableItemFigure {
  lazy val feed = new ResizeItemFeedbackFigure(this)
  def positionable = widget
  def resizable = widget
  def size = widget.size
  setOpaque(true)
  override def paintFigure(g: Graphics) {
    val rect = getClientArea()
    widget.component.setBounds(0, 0, rect.width, rect.height);
    widget.component.doLayout
    val aimage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB)
    val ag = aimage.createGraphics
    widget.component.paint(ag)
    val imageData = SWTUtils.convertAWTImageToSWT(aimage)
    val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(), imageData)
    g.drawImage(image, rect.x, rect.y)
    ag.dispose();
    image.dispose()
  }
}
class GUIModelView(viewer: GUIViewer, val model: GUIModel) extends AbstractModelView[GUIModel](viewer) {
  object widgetMapper extends ModelViewMapper[Widget, SwingFigure] {
    def modelSet = model.widgets
    def buildFigure(widget: Widget) = {
      new SwingFigure(viewer, widget)
    }
  }
  def update() {
    widgetMapper.update()
  }
}
object ExampleGUI {
  def simpleModel = {
    val m= new GUIModel
    val button = new JButton("hola")
    m.widgets += new Widget(button, Point(50,50), Dimension(60,60))
    m
  }
}
class GUIModel {
  var widgets = Set[Widget]()
}
class GUIController(val model: GUIModel) extends AbstractController[GUIModel] {
  private var viewModels = Buffer[GUIModelView]()
  def registerView(viewer: GUIViewer) = {
    val viewModel = new GUIModelView(viewer, model)
    viewModels += viewModel
    viewModel.update()
    viewModel
  }
  def updateViewers { viewModels foreach { _.update() } }
  def abortTools() { viewModels foreach { _.viewer.tool.state.abort() } }
}
class GUIViewer(parent: Composite, controller: GUIController) extends AbstractViewer(parent, controller) {
  /*TOOLS*/
  //val palette = new Palette(this, parent.getShell)
  var tool = new GUITool(this)
  /*MODEL*/
  override val modelView = controller.registerView(this)
  def model = controller.model
}
class GUITool(val viewer: GUIViewer) extends AbstractTool(viewer) 
