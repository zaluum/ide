package org.zaluum.nide.zge

import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{Ellipse, ColorConstants, Figure, ImageFigure}
import org.eclipse.draw2d.geometry.{Rectangle, Point => EPoint, Dimension => EDimension}
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.model.{Point => MPoint, Dimension, Vector2}
import org.zaluum.nide.newcompiler._
import scala.collection.mutable.Buffer

class PortFigure(val ipos: MPoint,
    val sym: PortSymbol,
    val in: Boolean,
    val valSym: Option[ValSymbol], 
    val container: BoxDefContainer)
    extends Ellipse with SimpleItem with RectFeedback{
  type T = Null
  def tree = null // ugly
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
class OpenPortDeclFigure(val tree: PortDef, val in:Boolean, val openBox: OpenBoxFigure) extends RectangleFigure with SimpleItem with RectFeedback {
  type T = PortDef
  def container = openBox.container
  def myLayer = container.portsLayer
  val size = Dimension(10, 10)
  // tree.extPos must be (0,relY)
  def xDisplacement = if (in) Vector2(0,0) else Vector2(openBox.size.w -10,0)
  def absDisplacement = Vector2(openBox.pos.x,openBox.pos.y)
  def relPos = tree.extPos + xDisplacement
  def pos = tree.extPos + xDisplacement + absDisplacement // abs coordinates
  override def update() {
    super.update()
    val sym = tree.symbol.asInstanceOf[PortSymbol]
    setBackgroundColor(Colorizer.color(sym.tpe))
    setForegroundColor(if (tree.dir==Shift) ColorConstants.yellow else ColorConstants.white)
    val valsym = openBox.valTree.symbol.asInstanceOf[ValSymbol]
    // external
    val extDisplacement = if (in) Vector2(-10,0) else Vector2(10,0)
    helpers += new PortFigure(getBounds.getCenter + extDisplacement, sym, in, Some(valsym), openBox.container)
    // internal
    def inDisplacement = if (in) Vector2(10,0) else Vector2(-10,0)
    helpers += new PortFigure(relPos + inDisplacement, sym, !in, None, openBox)
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

class PortDeclFigure(val tree: PortDef, val container: BoxDefContainer) extends ImageFigure with SimpleItem with RectFeedback {
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
    val image = container.viewer.imageFactory.get(PortDeclFigure.img(tree.dir)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    super.update()
    sym foreach { helpers += new PortFigure(position, _, tree.dir == In, None, container) }
  }
}
