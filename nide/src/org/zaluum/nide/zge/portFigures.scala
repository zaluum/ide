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
    setBackgroundColor(Colorizer.color(ps.pi.portSymbol.tpe))
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
  def update(ps: PortSide, left: Boolean) {
    this.tree = ps.pi.portSymbol.decl.asInstanceOf[PortDef];
    this.left = left
    val valSym = openBox.valSym 
    setBackgroundColor(Colorizer.color(sym.tpe))
    setForegroundColor(if (dir == Shift) ColorConstants.yellow else ColorConstants.white)
    updateSize()
    // external
    val extDisplacement = if (left) Vector2(-size.w, 0) else Vector2(size.w, 0)
    extPort.update(getBounds.getCenter + extDisplacement, ps)
    // internal
    def inDisplacement = if (left) Vector2(intPort.size.w/2, -openBox.getInsets.top + size.h/2) 
      else Vector2(-(size.w+intPort.size.w/2), -openBox.getInsets.top + size.h/2)
    intPort.update(relPos + inDisplacement, ps)
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
abstract class PortHolderFigure(val container: ContainerItem) extends AutoDisposeImageFigure with Item with HasPorts with RectFeedback {
  def ps: PortSide
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
class PortDeclFigure(val tree: PortDef, container: ContainerItem) extends PortHolderFigure(container) {
  def pos = tree.inPos
  def dir = tree.dir
  def sym = tree.symbol.asInstanceOf[PortSymbol]
  lazy val ps = sym.box.asInstanceOf[BoxTypeSymbol].thisVal.portSides.find {_.pi.portSymbol==sym}.get
  override def selectionSubject = Some(tree)
}
class PortSymbolFigure(val sym: PortSymbol, openBox: OpenBoxFigure) extends PortHolderFigure(openBox) {
  def pos = MPoint(openBox.getSize.w - Tool.gridSize*10, openBox.getSize.h - Tool.gridSize*5)
  def dir = sym.dir
  lazy val ps = sym.box.asInstanceOf[BoxTypeSymbol].thisVal.portSides.find {_.pi.portSymbol==sym}.get
}
