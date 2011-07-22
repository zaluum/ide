package org.zaluum.nide.zge

import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{ Ellipse, ColorConstants, Figure, ImageFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler.{ Point ⇒ MPoint, _ }
import scala.collection.mutable.Buffer

class PortFigure(val container:ContainerItem) extends Ellipse with Hover{
  def size = Dimension(6, 6)
  def pos = MPoint(ipos.x - size.w / 2, ipos.y - size.h / 2)
  def anchor = getBounds.getCenter
  private var ipos = MPoint(0, 0)
  var in = false
  var ps:PortSide = _
  def update(ipos: MPoint, ps: PortSide) {
    this.ps = ps
    this.ipos = ipos
    this.in = in
    setBounds(new Rectangle(pos.x,pos.y,size.w,size.h))
    setBackgroundColor(Colorizer.color(ps.pi.finalTpe))
    setBounds(new Rectangle(pos.x,pos.y,size.w,size.h))
  }
  var _hover = false
  def hover = _hover
  def hover_=(b:Boolean) = {
    _hover = b
    if (b) 
      setForegroundColor(ColorConstants.lightGray)
    else
      setForegroundColor(ColorConstants.black)
  }
  override def toString() =
    "PortFigure(" + ps + ")"
  setAntialias(1)
  hover = false
  //setAlpha(50)
  setOutline(true)
}
class OpenPortDeclFigure(val openBox: OpenBoxFigure) extends RectangleFigure with Item with HasPorts with RectFeedback {
  def extPos = tree.extPos
  def dir = tree.dir
  def sym = tree.symbol.asInstanceOf[PortSymbol]
  def container = openBox.container
  def myLayer = container.portsLayer
  val size = Dimension(openBox.getInsets.left,openBox.getInsets.left)
  
  // tree.extPos must be (0,relY)
  def xDisplacement = if (left) Vector2(0, 0) else Vector2(openBox.size.w - size.w, 0)
  def absDisplacement = Vector2(openBox.pos.x, openBox.pos.y)
  def relPos = extPos + xDisplacement
  def pos = extPos + xDisplacement + absDisplacement // abs coordinates
  var left = false
  var tree: PortDef = _
  override def selectionSubject = Some(tree)
  val extPort: PortFigure = new PortFigure(container)
  val intPort: PortFigure = new PortFigure(openBox)
  ports += extPort
  ports += intPort
  def blink(b:Boolean) {
    setXOR(b)
  }
  def update(intPs: PortSide, extPs:PortSide, left: Boolean) {
    this.tree = intPs.pi.portSymbol.get.decl.asInstanceOf[PortDef]; // XXX ??? real
    this.left = left
    setBackgroundColor(Colorizer.color(intPs.pi.finalTpe))
    setForegroundColor(if (dir == Shift) ColorConstants.yellow else ColorConstants.white)
    updateSize()
    // external
    val extDisplacement = if (left) Vector2(-size.w, 0) else Vector2(size.w, 0)
    extPort.update(getBounds.getCenter + extDisplacement, extPs)
    // internal
    def inDisplacement = if (left) Vector2(intPort.size.w/2, -openBox.getInsets.top + size.h/2) 
      else Vector2(-(size.w+intPort.size.w/2), -openBox.getInsets.top + size.h/2)
    intPort.update(relPos + inDisplacement, intPs)
  }
  this.setOpaque(true);
}

object PortDeclFigure {
  private def str(dir: PortDir) = dir match {
    case In ⇒ "In"
    case Out ⇒ "Out"
    case Shift ⇒ "Shift"
  }
}
abstract class PortHolderFigure(val container: ContainerItem, val ps:PortSide) extends AutoDisposeImageFigure with Item with HasPorts with RectFeedback {
  def myLayer = container.layer
  def pos: MPoint
  def dir: PortDir
  var size = Dimension(50, 20)
  val port = new PortFigure(container)
  def imageFactory = container.viewer.zproject.imageFactory
  ports += port
  def update() {
    disposeImage()
    val (image,newdesc) = container.viewer.zproject.imageFactory.portImg(dir)
    desc = newdesc
    setImage(image)
    size = Dimension(image.getBounds.width, image.getBounds.height)
    updateSize()
    val position = pos + (if (dir == In) Vector2(48, 8) else Vector2(0, 8))
    port.update(position, ps)
  }
  def blink(b:Boolean) {}

}
class PortDeclFigure(val tree: PortDef, ps:PortSide, container: ContainerItem) extends PortHolderFigure(container,ps) {
  def pos = tree.inPos
  def dir = tree.dir
  def sym = tree.symbol.asInstanceOf[PortSymbol]
  override def selectionSubject = Some(tree)
}
class PortSymbolFigure(val sym: PortSymbol, ps:PortSide, openBox: OpenBoxFigure) extends PortHolderFigure(openBox,ps) {
  def pos = MPoint(openBox.getSize.w - Tool.gridSize*10, openBox.getSize.h - Tool.gridSize*5)
  def dir = sym.dir
}
