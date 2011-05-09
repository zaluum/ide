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
  var sym: PortSymbol = _
  var fromSym: Symbol = _ 
  var portKey : PortKey= _
  def update(ipos: MPoint, sym: PortSymbol, fromSym: Symbol, in: Boolean) {
    this.ipos = ipos
    this.fromSym = fromSym
    this.sym = sym
    this.in = in
    portKey = fromSym match {
      case s:ValSymbol => ValPortKey(fromSym.name, sym.name, in)
      case b:BoxTypeSymbol => BoxPortKey(sym.name,in)
    }
    setBounds(new Rectangle(pos.x,pos.y,size.w,size.h))
    setBackgroundColor(Colorizer.color(sym.tpe))
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
    "PortFigure(" + sym + " " + fromSym + ")"
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
  val size = Dimension(10, 10)
  // tree.extPos must be (0,relY)
  def xDisplacement = if (left) Vector2(0, 0) else Vector2(openBox.size.w - 7, 0)
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
  def update(t: PortDef, left: Boolean) {
    this.tree = t
    this.left = left
    setBackgroundColor(Colorizer.color(sym.tpe))
    setForegroundColor(if (dir == Shift) ColorConstants.yellow else ColorConstants.white)
    updateSize()
    val valsym = openBox.valDef.symbol.asInstanceOf[ValSymbol]
    // external
    val extDisplacement = if (left) Vector2(-7, 0) else Vector2(7, 0)
    extPort.update(getBounds.getCenter + extDisplacement, sym, valsym, left)
    // internal
    def inDisplacement = if (left) Vector2(7, 0) else Vector2(-7, 0)
    intPort.update(relPos + inDisplacement, sym, openBox.symbol, !left)
  }
  this.setOpaque(true);
}

object PortDeclFigure {
  private def str(dir: PortDir) = dir match {
    case In ⇒ "In"
    case Out ⇒ "Out"
    case Shift ⇒ "Shift"
  }
  def img(dir: PortDir) = "org/zaluum/nide/icons/portDecl" + str(dir) + ".png"
}
abstract class PortHolderFigure(val container: ContainerItem) extends AutoDisposeImageFigure with Item with HasPorts with RectFeedback {
  def sym: PortSymbol
  def myLayer = container.layer
  def pos: MPoint
  def dir: PortDir
  var size = Dimension(50, 20)
  val port = new PortFigure(container)
  ports += port
  def update() {
    disposeImage()
    val image = container.viewerResources.imageFactory.load(PortDeclFigure.img(dir)).get
    setImage(image)
    size = Dimension(image.getBounds.width, image.getBounds.height)
    updateSize()
    val position = pos + (if (dir == In) Vector2(48, 8) else Vector2(0, 8))
    port.update(position, sym, container.symbol, dir == In)
  }
  def blink(b:Boolean) {}

}
class PortDeclFigure(val tree: PortDef, container: ContainerItem) extends PortHolderFigure(container) {
  def pos = tree.inPos
  def dir = tree.dir
  def sym = tree.symbol.asInstanceOf[PortSymbol]
  override def selectionSubject = Some(tree)
}
class PortSymbolFigure(val sym: PortSymbol, openBox: OpenBoxFigure) extends PortHolderFigure(openBox) {
  def pos = MPoint(openBox.getSize.w - 60, openBox.getSize.h - 30)
  def dir = sym.dir
}
