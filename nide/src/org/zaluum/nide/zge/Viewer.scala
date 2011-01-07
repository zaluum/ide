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

class Viewer(parent: Composite, val controller:Controller) {
  
  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  canvas.setScrollBarVisibility(FigureCanvas.AUTOMATIC)

  val feedbackLayer = new FreeformLayer
  val portsLayer = new FreeformLayer
  val layer = new FreeformLayer
  layer.setOpaque(true);
  layer.setBackgroundColor(ColorConstants.white)
  //layer.setLayoutManager(new FreeformLayout)
  val viewport = new FreeformViewport();
  val innerLayers = new ScalableFreeformLayeredPane();
  innerLayers.add(layer)
  innerLayers.add(portsLayer)
  innerLayers.add(feedbackLayer)
  viewport.setContents(innerLayers);
  canvas.setViewport(viewport)
  var tool= new MoveTool(this)
  val modelView = controller.registerView(this)
  def dispose() {
    canvas.dispose()
  }
  def setCursor(cursor:Cursor) {
    canvas.setCursor(cursor)
  }
  def findDeepAt(container:IFigure, p: Point) = {
    Option( container.findFigureAt(p.x, p.y)) filter (_!=container)     
  }
  def findShallowAt(container:IFigure, p : Point ) = {
    import scala.collection.JavaConversions._
    container.getChildren.asInstanceOf[java.util.List[IFigure]] find { _ .containsPoint(p) };
  }
  def figureAt(p:Point) = findShallowAt(layer,p) map { case (bf : BoxFigure) => bf } 
  def feedbackAt(p:Point) = findDeepAt(feedbackLayer,p)
  
  val marquee = new RectangleFigure
  marquee.setFill(false)
  marquee.setLineStyle(SWT.LINE_DASH);
  def showMarquee() { feedbackLayer.add(marquee) }
  def moveMarquee(r:Rectangle) { marquee.setBounds(r)}
  def hideMarquee() { feedbackLayer.remove(marquee) }
  UIManager.setLookAndFeel("javax.swing.plaf.synth.SynthLookAndFeel");
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
class ImageBoxFigure(val box:Box,val viewer:Viewer, val image:Image) extends ImageFigure(image) with BoxFigure with WithPorts
class SwingBoxFigure(val viewer:Viewer,val box:Box, c: JComponent) 
  extends SwingFigure(c) with BoxFigure

trait BoxFigure extends Figure{
  def viewer:Viewer
  val box:Box
  lazy val feed = new ResizeFeedbackFigure(this)
  
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
    val rect = new Rectangle(box.pos._1,box.pos._2,box.size._1,box.size._2)
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
class ConnectionFigure(val c: Connection) extends Polyline {
  def update() {
    
  }
}
class ModelView(val viewer:Viewer,val model:Model) {
  var selected  = Set[BoxFigure]()
  val img = new Image(viewer.canvas.getDisplay,"icons/op.png")
  private var viewMap = Map[Box,BoxFigure]() 
  private var connectionMap = Map[Connection,ConnectionFigure]()
  def update() {
    val removed =  viewMap.keySet -- model.boxes
    val added =  model.boxes -- viewMap.keys
    removed foreach {b => viewMap(b).hide; viewMap -= b}
    for (box <- added) {
      //val boxview = new SwingBoxFigure(viewer,box, new JButton("text"))
      val boxview = new ImageBoxFigure(box,viewer,img)
      viewMap += (box -> boxview)
      boxview.show
    }
    viewMap.values foreach { _.update() }
  }
  def select(box : BoxFigure) {
   if (!selected(box)) {
     selected += box
     box.showFeedback
   }
  }
  def deselect(box: BoxFigure) {
    if (selected(box)){
      box.hideFeedback
      selected -= box
    }
  }
  def deselectAll() {
    selected foreach {deselect(_)}
  }
  def toggleSelection(box:BoxFigure) {
    if (selected(box)) deselect(box)
    else select(box)
  }
  def updateSelection(figs: Set[BoxFigure], shift:Boolean) {
    if (shift) {
      figs foreach { toggleSelection(_) }
    } else {
      deselectAll()
      figs foreach { select(_) }
    }
  }
}