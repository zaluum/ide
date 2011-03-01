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
  def size = Dimension(10, 10)
  def pos = MPoint(ipos.x - size.w/2, ipos.y-size.h / 2)
  val highlight = ColorConstants.blue
  val normal = ColorConstants.gray
  def anchor = getBounds.getCenter
  override def showFeedback() {
    setBackgroundColor(highlight)
  }
  override def hideFeedback() {
    setBackgroundColor(normal)
  }
  override def toString() = 
    "PortFigure(" + sym + " " + valSym + ")" 
  setBackgroundColor(normal)
  setAntialias(1)
  setAlpha(50)
  setOutline(false)
}
class OpenPortSymbolFigure(val sym:PortSymbol, left:Boolean, openBox:OpenBoxFigure) extends OpenPortFigure(left,openBox) with SymbolItem {
  type S = PortSymbol
  def extPos = sym.extPos
  def dir = sym.dir 
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
  def xDisplacement = if (left) Vector2(0,0) else Vector2(openBox.size.w -10,0)
  def absDisplacement = Vector2(openBox.pos.x,openBox.pos.y)
  def relPos = extPos + xDisplacement
  def pos = extPos + xDisplacement + absDisplacement // abs coordinates
  override def update() {
    super.update()
    setBackgroundColor(Colorizer.color(sym.tpe))
    setForegroundColor(if (dir==Shift) ColorConstants.yellow else ColorConstants.white)
    val valsym = openBox.valTree.symbol.asInstanceOf[ValSymbol]
    // external
    val extDisplacement = if (left) Vector2(-10,0) else Vector2(10,0)
    helpers += new PortFigure(getBounds.getCenter + extDisplacement, sym, left, Some(valsym), openBox.container)
    // internal
    def inDisplacement = if (left) Vector2(10,0) else Vector2(-10,0)
    helpers += new PortFigure(relPos + inDisplacement, sym, !left, None, openBox)
  }
  setForegroundColor(ColorConstants.white)
  //setBackgroundColor(ColorConstants.gray)
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

class PortDeclFigure(val tree: PortDef, val container: BoxDefContainer) extends ImageFigure with SimpleItem with TreeItem with RectFeedback {
  type T = PortDef
  def sym = tree.symbol match {
    case NoSymbol ⇒ None
    case p: PortSymbol ⇒ Some(p)
  }
  def myLayer = container.layer
  def pos = tree.pos
  var size = Dimension(50, 20)

  override def update() {
    val position = tree.inPos + (if (tree.dir == In) Vector2(48, 8) else Vector2(0, 8))
    val image = container.viewerResources.imageFactory.get(PortDeclFigure.img(tree.dir)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    super.update()
    sym foreach { helpers += new PortFigure(position, _, tree.dir == In, None, container) }
  }
}
