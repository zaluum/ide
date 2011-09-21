package org.zaluum.nide.zge

import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Ellipse
import org.eclipse.draw2d.RectangleFigure
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.{ Point ⇒ MPoint }
import org.zaluum.nide.compiler.PortDef
import org.zaluum.nide.compiler.PortDir
import org.zaluum.nide.compiler.PortSide
import org.zaluum.nide.compiler.PortSymbol
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.Vector2
import draw2dConversions._

class PortFigure(val container: ContainerItem) extends Ellipse with Hover {
  var size = Dimension(5, 5)
  def pos = MPoint(ipos.x - size.w / 2, ipos.y - size.h / 2)
  def anchor = getBounds.getCenter
  private var ipos = MPoint(0, 0)
  var in = false
  var ps: PortSide = _
  def isBad = ps != null && ps.tpe.isEmpty
  def update(ipos: MPoint, ps: PortSide) {
    this.ps = ps
    this.ipos = ipos
    this.in = in
    ps.pi.tpe match {
      case Some(t) ⇒
        setBackgroundColor(Colorizer.color(ps.pi.tpe))
        setLineWidthFloat(1)
        size = Dimension(5, 5)
        updateHoverColor()
      case None ⇒
        setBackgroundColor(ColorConstants.white)
        setLineWidthFloat(1.5F)
        size = Dimension(7, 7)
    }
    setBounds(new Rectangle(pos.x, pos.y, size.w, size.h))
  }
  var _hover = false
  def hover = _hover
  def hover_=(b: Boolean) = {
    _hover = b
    updateHoverColor()
  }
  def updateHoverColor() {
    if (isBad) {
      setForegroundColor(ColorConstants.red)
    } else {
      if (_hover)
        setForegroundColor(ColorConstants.lightGray)
      else
        setForegroundColor(getBackgroundColor())
    }
  }
  override def toString() =
    "PortFigure(" + ps + ")"
  setAntialias(1)
  hover = false
  //setAlpha(50)
  setOutline(true)
}

abstract class OpenPortBaseFigure(val openBox: OpenBoxFigure) extends RectangleFigure with OverlappedItem with HasPorts with RectFeedback {
  def dir: PortDir
  def myLayer = container.portsLayer
  val size = Dimension(openBox.getInsets.left, openBox.getInsets.left)

  // tree.extPos must be (0,relY)
  def constantDisplacement = if (left) Vector2(0, 0) else Vector2(openBox.size.w - size.w, 0)
  var left = false
  val extPort: PortFigure = new PortFigure(container)
  val intPort: PortFigure = new PortFigure(openBox)
  ports += extPort
  ports += intPort
  def blink(b: Boolean) {
    setXOR(b)
  }
  protected def update(intPs: PortSide, extPs: PortSide, left: Boolean) {
    this.left = left
    setBackgroundColor(Colorizer.color(intPs.pi.tpe))
    setForegroundColor(if (dir == Shift) ColorConstants.yellow else ColorConstants.white)
    updateSize()
    // external
    val extDisplacement = if (left) Vector2(-size.w, 0) else Vector2(size.w, 0)
    extPort.update(getBounds.getCenter + extDisplacement, extPs)
      // internal
      def inDisplacement = if (left) Vector2(intPort.size.w / 2, -openBox.getInsets.top + size.h / 2)
      else Vector2(-(size.w + intPort.size.w / 2), -openBox.getInsets.top + size.h / 2)
    intPort.update(relPos + inDisplacement, intPs)
  }
  this.setOpaque(true);
}
class OpenPortDeclFigure(openBox: OpenBoxFigure) extends OpenPortBaseFigure(openBox) {
  def extPos = tree.extPos
  def dir = tree.dir
  var tree: PortDef = _
  override def selectionSubject = Some(tree)
  override def update(intPs: PortSide, extPs: PortSide, left: Boolean) {
    this.tree = intPs.pi.portSymbol.get.decl.asInstanceOf[PortDef]; // XXX ??? real
    super.update(intPs, extPs, left)
  }
}
class OpenPortFixedFigure(openBox: OpenBoxFigure) extends OpenPortBaseFigure(openBox) {
  var extPos = MPoint(0, 0)
  var dir: PortDir = _
  def update(intPs: PortSide, extPs: PortSide, left: Boolean, dir: PortDir, extPos: MPoint) {
    this.extPos = extPos
    this.dir = dir
    update(intPs, extPs, left)
  }
}

abstract class PortHolderFigure(val container: ContainerItem, val ps: PortSide) extends RectangleFigure
    with TextEditFigure with Item with HasPorts with RectFeedback {
  setLineWidthFloat(2)
  def myLayer = container.layer
  def pos: MPoint
  def dir: PortDir
  def size = preferredSize
  val port = new PortFigure(container)
  def imageFactory = container.viewer.zproject.imageFactory
  ports += port
  def update() {
    updateText() // XXX super.update; updateText doesn't show text. Why?
    setForegroundColor(Colorizer.color(ps.pi.tpe))
    updateSize()
    val position = pos + (if (dir == In) Vector2(size.w, 8) else Vector2(0, 8))
    port.update(position, ps)
  }
  def blink(b: Boolean) {}

}
object PortDeclFigure {
  private def str(dir: PortDir) = dir match {
    case In    ⇒ "In"
    case Out   ⇒ "Out"
    case Shift ⇒ "Shift"
  }
}

class PortDeclFigure(val tree: PortDef, ps: PortSide, container: ContainerItem) extends PortHolderFigure(container, ps) {
  def pos = tree.inPos
  def dir = tree.dir
  def sym = tree.sym
  def text = ps.name.str
  override def selectionSubject = Some(tree)

}
class PortSymbolFigure(ps: PortSide, openBox: OpenBoxFigure) extends PortHolderFigure(openBox, ps) {
  def pos = MPoint(openBox.getClientArea.getBottomRight.x - size.w - 1, openBox.getClientArea.getBottomRight.y - size.h - 1)
  def dir = ps.pi.dir
  def text = ps.name.str
}
