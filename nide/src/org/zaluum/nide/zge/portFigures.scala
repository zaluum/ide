package org.zaluum.nide.zge
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
    extends Ellipse with Item with RectFeedback{
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
class OpenPortDeclFigure(val tree: PortDef, val openBox: OpenBoxFigure) extends Item with RectFeedback {
  type T = PortDef
  def container = openBox.container
  def myLayer = container.portsLayer
  val size = Dimension(10, 10)
  def pos = openBox.pos + tree.extPos
    override def update() {
    super.update()
    val sym = tree.symbol.asInstanceOf[PortSymbol]
    val valsym = openBox.valTree.symbol.asInstanceOf[ValSymbol]
    helpers += new PortFigure(getBounds.getCenter + Vector2(-10, 0), sym, tree.dir == In, Some(valsym), openBox.container)
    helpers += new PortFigure(tree.extPos + Vector2(10, 0), sym, tree.dir == In, None, openBox)
  }
  setBackgroundColor(ColorConstants.yellow)
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

class PortDeclFigure(val tree: PortDef, val container: BoxDefContainer) extends ImageFigure with Item with RectFeedback {
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
