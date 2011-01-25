package org.zaluum.nide.zge

import org.zaluum.nide.model.Point
import org.zaluum.nide.model.Positionable
import org.zaluum.nide.model.Dimension
import org.zaluum.nide.model.Resizable
import scala.collection.mutable.Buffer
import javax.swing.JComponent
import java.awt.BorderLayout
import javax.swing.JPanel
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.Figure
import java.awt.{Graphics => AG}
import java.awt.image.BufferedImage
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Composite

class Widget(var component : JComponent, var pos:Point, var size:Dimension) extends Positionable with Resizable 

class SwingFigure(val viewer:Viewer,val widget:Widget) extends Figure with ResizableItemFigure {
  lazy val feed = new ResizeItemFeedbackFigure(this)
  def positionable = widget
  def resizable = widget
  def size = widget.size
  setOpaque(true)
  override def paintFigure(g:Graphics) {
    val rect = getClientArea()
    widget.component.setBounds(0,0,rect.width,rect.height);
    widget.component.doLayout
    val aimage = new BufferedImage(rect.width,rect.height,BufferedImage.TYPE_INT_RGB)
    val ag = aimage.createGraphics
    widget.component.paint(ag)
    val imageData = SWTUtils.convertAWTImageToSWT(aimage)
    val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(),imageData)
    g.drawImage(image,rect.x,rect.y)
    ag.dispose();    
    image.dispose()
  } 
}
class GUIModelView(viewer:GUIViewer, val model:GUIModel) extends AbstractModelView[GUIModel](viewer){
  def update(){}
}
class GUIModel
class GUIController(val model:GUIModel) extends AbstractController[GUIModel]{
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
class GUIViewer(parent:Composite, controller:GUIController) extends AbstractViewer(parent,controller){
  /*TOOLS*/
  //val palette = new Palette(this, parent.getShell)
  var tool = new GUITool(this)
  /*MODEL*/
  override val modelView = controller.registerView(this)
  def model = controller.model
}
class GUITool(val viewer:GUIViewer) extends AbstractTool(viewer) {
  
}