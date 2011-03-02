package org.zaluum.nide.zge

import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{Ellipse, ColorConstants, Figure, ImageFigure}
import org.eclipse.draw2d.geometry.{Rectangle, Point => EPoint, Dimension => EDimension}
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler.{Point => MPoint,_}
import scala.collection.mutable.Buffer

class PortFigure(val ipos: MPoint,
    val sym: PortSymbol,
    val in: Boolean,
    val valSym: Option[ValSymbol], 
    val container: BoxDefContainer)
    extends Ellipse with SimpleItem with RectFeedback{
  def myLayer = container.portsLayer
  def size = Dimension(6, 6)
  def pos = MPoint(ipos.x - size.w/2, ipos.y-size.h / 2)
  def anchor = getBounds.getCenter
  override def showFeedback() {
    setForegroundColor(ColorConstants.lightGray)
    setBackgroundColor(Colorizer.color(sym.tpe))
  }
  override def hideFeedback() {
    setForegroundColor(ColorConstants.black)
    setBackgroundColor(Colorizer.color(sym.tpe))
  }
  override def toString() = 
    "PortFigure(" + sym + " " + valSym + ")" 
  setAntialias(1)
  hideFeedback
  //setAlpha(50)
  setOutline(true)
}
class OpenPortDeclFigure(val tree: PortDef, left:Boolean, openBox: OpenBoxFigure) extends OpenPortFigure(left,openBox) with TreeItem {
  type T = PortDef
  def extPos = tree.extPos
  def dir = tree.dir
  def sym = tree.symbol.asInstanceOf[PortSymbol]
}

abstract class OpenPortFigure(val left:Boolean, val openBox: OpenBoxFigure) extends RectangleFigure with SimpleItem with RectFeedback {
  def container = openBox.container
  def myLayer = container.portsLayer
  val size = Dimension(10, 10)
  def extPos : MPoint
  def dir :PortDir
  def sym : PortSymbol
  // tree.extPos must be (0,relY)
  def xDisplacement = if (left) Vector2(0,0) else Vector2(openBox.size.w -7,0)
  def absDisplacement = Vector2(openBox.pos.x,openBox.pos.y)
  def relPos = extPos + xDisplacement
  def pos = extPos + xDisplacement + absDisplacement // abs coordinates
  override def update() {
    super.update()
    setBackgroundColor(Colorizer.color(sym.tpe))
    setForegroundColor(if (dir==Shift) ColorConstants.yellow else ColorConstants.white)
    val valsym = openBox.valTree.symbol.asInstanceOf[ValSymbol]
    // external
    val extDisplacement = if (left) Vector2(-7,0) else Vector2(7,0)
    helpers += new PortFigure(getBounds.getCenter + extDisplacement, sym, left, Some(valsym), openBox.container)
    // internal
    def inDisplacement = if (left) Vector2(7,0) else Vector2(-7,0)
    helpers += new PortFigure(relPos + inDisplacement, sym, !left, None, openBox)
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
abstract class PortHolderFigure(val container:BoxDefContainer) extends ImageFigure with SimpleItem with RectFeedback {
  def sym : PortSymbol 
  def myLayer = container.layer
  def pos : MPoint 
  def dir : PortDir
  var size = Dimension(50, 20)
  override def update() {
    val position = pos + (if (dir == In) Vector2(48, 8) else Vector2(0, 8))
    val image = container.viewerResources.imageFactory.get(PortDeclFigure.img(dir)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    super.update()
    helpers += new PortFigure(position, sym, dir == In, None, container) 
  }
  
}
class PortDeclFigure(val tree: PortDef, container: BoxDefContainer) extends PortHolderFigure(container) with TreeItem  {
  type T = PortDef
  def pos = tree.inPos
  def dir = tree.dir 
  def sym = tree.symbol.asInstanceOf[PortSymbol]
}
class PortSymbolFigure(val sym:PortSymbol, openBox:OpenBoxFigure) extends PortHolderFigure(openBox) with SymbolItem {
  type S = PortSymbol
  def pos = MPoint(openBox.getSize.w -60, openBox.getSize.h -30 )
  def dir = sym.dir 
}
