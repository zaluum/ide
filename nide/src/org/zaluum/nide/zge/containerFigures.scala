package org.zaluum.nide.zge

import org.eclipse.swt.widgets.Text
import org.eclipse.swt.events.{ FocusListener, FocusEvent }
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.jface.viewers.ICellEditorListener
import org.eclipse.draw2d.text.FlowPage
import org.eclipse.draw2d.text.TextFlow
import org.eclipse.draw2d.geometry.Point
import org.eclipse.draw2d.GroupBoxBorder
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.LayeredPane
import org.eclipse.draw2d.Layer
import org.eclipse.draw2d.LineBorder
import draw2dConversions._
import org.eclipse.draw2d.{ FreeformLayer, Ellipse, ColorConstants, Figure, ImageFigure, Polyline, ScalableFreeformLayeredPane, IFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler.{ Point ⇒ MPoint, _ }
import scala.collection.mutable.Buffer
import scala.collection.JavaConversions._
import RichFigure._
trait Container extends IFigure {
  def viewer : Viewer
  def viewerResources: ViewerResources
  def layer: Figure
  val helpers: Buffer[ShowHide]
  def feedbackLayer: Figure
  def itemOrLineAt(p: Point, debug: Boolean = false) = this.findDeepAt(point(p), 0, debug) {
    case bf: Item ⇒ bf
    case l: LineFigure ⇒ l
  }
  def clear() {
    layer.removeAll()
    feedbackLayer.removeAll()
  }

}
trait BoxDefContainer extends Container {
  def boxDef: BoxDef
  def connectionsLayer: Figure
  def portsLayer: Figure
  private def portFigures = portsLayer.getChildren.collect { case p: PortFigure ⇒ p }
  def findPortFigure(boxName: Name, portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒
      p.valSym match {
        case Some(valSym) ⇒ (valSym.name == boxName && p.sym.name == portName && p.in == in)
        case None ⇒ false
      }
    }
  }
  def findPortFigure(portName: Name, in: Boolean): Option[PortFigure] = {
    portFigures find { p ⇒ p.valSym.isEmpty && p.sym.name == portName && p.in == in }
  }
  override def clear() {
    super.clear()
    connectionsLayer.removeAll()
    portsLayer.removeAll()
  }
  def owner: Symbol
  def populateFigures() {
    boxDef.children foreach {
      _ match {
        case EmptyTree ⇒
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) ⇒
          v.scope.lookupBoxTypeLocal(typeName) match {
            case Some(tpe) ⇒
              helpers += new OpenBoxFigure(v,
                tpe.decl.asInstanceOf[BoxDef],
                v.symbol.owner,
                BoxDefContainer.this,
                viewer,
                viewerResources)
            case None ⇒
              v.params.headOption match {
                case Some(p:Param) => 
                  helpers += new DirectValFigure(v, p, BoxDefContainer.this)
                case _ =>
                  helpers += new ImageValFigure(v, BoxDefContainer.this)
              }
          }
        case _ ⇒
      }
    }
  }
  def populateConnections() {
    boxDef.connections foreach {
      _ match {
        case c:ConnectionDef ⇒
          helpers += new ConnectionFigure(c, BoxDefContainer.this)
        case _ ⇒
      }
    }
  }
  def populate() {
    populateFigures()
    // create connections (requires figures)
    populateConnections()
  }
}
import scala.collection.JavaConversions._
trait Transparent extends Figure {
  override def containsPoint(x: Int, y: Int) = {
    val pt = new Point(x, y)
    translateFromParent(pt);
    if (getClientArea.contains(pt)) {
      getChildren.asInstanceOf[java.util.List[IFigure]].reverse exists (_.containsPoint(pt.x, pt.y))
    } else {
      getBounds.contains(x, y)
    }
  }
}
class OpenBoxFigure(
  val valTree: ValDef,
  val boxDef: BoxDef,
  val owner: Symbol,
  val container: BoxDefContainer,
  val viewer: Viewer,
  val viewerResources: ViewerResources) extends SimpleItem with TreeItem with ResizableFeedback with BoxDefContainer with Transparent {
  // Item
  type T = ValDef
  def tree = valTree
  def myLayer = container.layer
  def pos = tree.pos
  def size = valTree.size getOrElse Dimension(100, 100)
  // layers
  val inners = new LayeredPane
  val layer = new Layer
  val portsLayer = new Layer
  val connectionsLayer = new Layer
  val feedbackLayer = new Layer
  // BoxDefContainer
  override def useLocalCoordinates = true
  override def populateFigures() {
    super.populateFigures()
    boxDef.children foreach {
      _ match {
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
          def newFig(left: Boolean) = helpers += new OpenPortDeclFigure(p, left, OpenBoxFigure.this)
          in match {
            case In ⇒ newFig(true)
            case Out ⇒ newFig(false)
            case Shift ⇒ newFig(true); newFig(false)
          }
        case _ ⇒
      }
    }
    boxDef.symbol match {
      case s: BoxTypeSymbol ⇒
        for (sup ← s.superSymbol; p ← sup.ports.values) {
          p match {
            case p: PortSymbol ⇒
              helpers += new PortSymbolFigure(p, OpenBoxFigure.this)
          }
        }
    }
  }
  override def paintClientArea(graphics: Graphics) {
    val rect = new Rectangle
    if (useLocalCoordinates()) {
      graphics.translate(
        getBounds().x + getInsets().left,
        getBounds().y + getInsets().top);
    }
    graphics.clipRect(getClientArea(rect));
    graphics.pushState();
    graphics.setAlpha(25);
    graphics.setBackgroundColor(ColorConstants.lightGray);
    graphics.fillRectangle(rect);
    graphics.popState();
    graphics.restoreState();
    super.paintClientArea(graphics)
  }
  override def update() {
    super.update()
    populate()
    inners.setSize(this.getSize)
  }

  inners.add(layer)
  inners.add(portsLayer)
  inners.add(connectionsLayer)
  inners.add(feedbackLayer)
  add(inners);
  setBorder(new LineBorder(ColorConstants.gray, 5))

}