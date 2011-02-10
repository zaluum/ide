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
  val valSym: Option[ValSymbol], val container: BoxDefContainer)
  extends Ellipse with Item with RectFeedback{
  type T = Null
  def tree = null
  def myLayer = container.portsLayer
  def size = Dimension(10, 10)
  def pos = ipos + Vector2(-getBounds.width / 2, -getBounds.height / 2)
  setAntialias(1)
  setAlpha(50)
  setOutline(false)
  val highlight = ColorConstants.blue
  val normal = ColorConstants.gray
  setBackgroundColor(normal)
  override def showFeedback() {
    setBackgroundColor(highlight)
  }
  override def hideFeedback() {
    setBackgroundColor(normal)
  }
  def anchor = getBounds.getCenter
}
class OpenPortDeclFigure(val tree: PortDef, val openBox: OpenBoxFigure) extends Item with RectFeedback {
  type T = PortDef
  setSize(10, 10)
  setBackgroundColor(ColorConstants.yellow)
  this.setOpaque(true);
  def container = openBox.container
  def myLayer = container.portsLayer
  val size = Dimension(10, 10)
  def sym = tree.symbol.asInstanceOf[PortSymbol]
  def valsym = openBox.valTree.symbol.asInstanceOf[ValSymbol]
  def pos = openBox.pos
  def portPos = pos + tree.extPos
  val extPortFig = new PortFigure(portPos + Vector2(0, 5), sym, tree.dir == In, Some(valsym), openBox.container)
  val intPortFig = new PortFigure(tree.extPos + Vector2(5, 0), sym, tree.dir == In, Some(valsym), openBox)
  override def update() {
    items += extPortFig
    items += intPortFig
    super.update()
  }
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
  def position = tree.inPos + (if (tree == In) Vector2(48, 8) else Vector2(0, 8))

  override def update() {
    val image = container.viewer.imageFactory.get(PortDeclFigure.img(tree.dir)).get
    setImage(image)
    size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
    sym foreach { items += new PortFigure(position, _, tree.dir == In, None, container) }
    super.update()
  }
}

