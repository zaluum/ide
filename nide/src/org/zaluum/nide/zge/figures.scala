package org.zaluum.nide.zge


import org.eclipse.draw2d.Polyline
import org.eclipse.swt.graphics.Image
import org.eclipse.draw2d.ImageFigure
import org.eclipse.draw2d.GridLayout
import org.eclipse.draw2d.FreeformLayout
import org.eclipse.draw2d.XYLayout
import org.eclipse.draw2d.FlowLayout
import scala.collection.mutable.Buffer
import javax.swing.UIManager
import javax.swing.JComponent
import javax.swing.JButton
import javax.swing.JPanel
import org.eclipse.swt.SWT
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.IFigure
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{FigureCanvas, ScalableFreeformLayeredPane, FreeformLayer, FreeformViewport, LightweightSystem, Ellipse, ColorConstants, Figure}
import org.eclipse.draw2d.geometry.{ Rectangle, Point, Dimension }
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.model._ 

trait BoxFigure extends Figure{
  def viewer:Viewer
  val box:Box
  var boxClass:Option[BoxClass]
  lazy val feed = new BoxFeedbackFigure(this)
  def size : (Int,Int)
  def showFeedback() {
    viewer.feedbackLayer.add(feed)
    update()
  }
  def hideFeedback {
    viewer.feedbackLayer.remove(feed)
  }
  def show() {
    viewer.layer.add(this)
    update()
  }
  def hide(){
   viewer.layer.remove(this) 
  }
  def update() {
    val rect = new Rectangle(box.pos._1,box.pos._2,size._1,size._2)
    setBounds(rect)
    feed.setInnerBounds(rect)
  }
  def moveDeltaFeed(delta : (Int,Int)) {
   val loc = new Point(box.pos._1 + delta._1, box.pos._2 + delta._2)
   feed.setInnerLocation(loc)
  }
  def resizeDeltaFeed(delta : (Int,Int), handle : HandleRectangle) {
    feed.setInnerBounds(handle.deltaAdd(delta,getBounds))
  }
}

trait WithPorts extends BoxFigure{
  var ports = Map[Port,PortFigure]()
  private var showing = false
  override def show() {
    super.show()
    ports.values foreach { _.show()}
    showing = true
  }
  override def hide() {
    super.hide()
    ports.values foreach { _.hide()}
    showing = false
  }
  override def update() {
    super.update()
    val removed = ports.keySet -- box.ports
    val added = box.ports -- ports.keySet
    for (p <- removed) {
      val pf = ports(p)
      if (showing) pf.hide()
      ports -= p
    }
    for (p <- added) {
      val pf = new PortFigure(this,p,viewer)
      ports += (p-> pf)
      if (showing) pf.show()
    }
    ports.values foreach { _.update() }
  }
}

class ImageBoxFigure(val box:Box,var boxClass:Option[BoxClass],val viewer:Viewer, val image:Image) extends ImageFigure(image) with BoxFigure with WithPorts{
  def size =( image.getBounds.width, image.getBounds.height)
}
class SwingBoxFigure(val viewer:Viewer,val box:Box, c: JComponent)  extends SwingFigure(c) with BoxFigure{
  def size = (50,50) //FIXME
  var boxClass:Option[BoxClass] = None // FIXME
}

class ConnectionFigure(val c: Connection) extends Polyline {
  def update() {
    
  }
}
