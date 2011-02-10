package org.zaluum.nide.zge

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
import org.zaluum.nide.model.{ Point ⇒ MPoint, Dimension, Vector2, Resizable, Line, Positionable, Route }
import org.zaluum.nide.newcompiler.{ Tree, PortSymbol, PortDef, ConnectionDef, ValSymbol, ValDef, BoxTypeSymbol, NoSymbol, PortRef, ValRef, EmptyTree, ThisRef, In, PortDir, Out, Shift, BoxDef, Traverser, Symbol, Name }
import scala.collection.mutable.Buffer

trait BoxDefContainer extends IFigure {
  def boxDef: BoxDef
  def viewer: TreeViewer // TODO only for image factory... remove?
  def layer: Figure
  def feedbackLayer: Figure
  def connectionsLayer: Figure
  def portsLayer: Figure
  def findDeepAt(container: IFigure, p: EPoint) = {
    Option(container.findFigureAt(p.x, p.y)) filter (_ != container)
  }
  import scala.collection.JavaConversions._
  private def findShallowAt(container: IFigure, p: EPoint) = {
    import scala.collection.JavaConversions._
    container.getChildren.asInstanceOf[java.util.List[IFigure]] find { _.containsPoint(p) };
  }
  def figureAt(p: EPoint) = findShallowAt(layer, p) map { case (bf: Item) ⇒ bf }
  def feedbackAt(p: EPoint) = findDeepAt(feedbackLayer, p)
  def lineAt(p: EPoint) = findDeepAt(connectionsLayer, p) map { case l: LineFigure ⇒ l }
  private def portFigures = portsLayer.getChildren.collect { case p: PortFigure ⇒ p }
  def findPortFigure(boxName: Name, portName: Name, in: Boolean): Option[PortFigure] =
    portFigures find { p ⇒
      p.valSym match {
        case Some(valSym) ⇒ (valSym.name == boxName && p.sym.name == portName && p.in == in)
        case None ⇒ false
      }
    }
  def findPortFigure(portName: Name, in: Boolean): Option[PortFigure] =
    portFigures find { p ⇒ p.valSym.isEmpty && p.sym.name == portName && p.in == in }

  def owner: Symbol
  def clear() {
    layer.removeAll()
    feedbackLayer.removeAll()
    connectionsLayer.removeAll()
    portsLayer.removeAll()
  }
  def populateFigures() {
    boxDef.children foreach {
      _ match {
        case EmptyTree ⇒
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
          v.scope.lookupBoxTypeLocal(typeName) match {
            case Some(tpe) ⇒
              new OpenBoxFigure(v,
                tpe.decl.asInstanceOf[BoxDef],
                v.symbol.owner,
                BoxDefContainer.this,
                viewer).show()
            case None ⇒
              new ImageValFigure(v, BoxDefContainer.this).show()
          }
        case _ ⇒
      }
    }
  }
  def populateConnections() {
    boxDef.connections foreach {
      _ match {
        case c@ConnectionDef(a, b) ⇒
          new ConnectionFigure(c, BoxDefContainer.this).show()
        case _ ⇒
      }
    }
  }
  def populate() {
    clear()
    populateFigures()
    // create connections (need to find figures positions)
    populateConnections()
  }
}
class OpenBoxFigure(
  val valTree: ValDef,
  val boxDef: BoxDef,
  val owner: Symbol,
  val container: BoxDefContainer,
  val viewer: TreeViewer) extends Item with ResizableFeedback with BoxDefContainer {
  type T = ValDef
  def myLayer = container.layer
  def tree = valTree
  def pos = tree.pos
  def size = valTree.size getOrElse Dimension(100, 100)
  val layer = new Layer
  val portsLayer = new Layer
  val connectionsLayer = new Layer
  val feedbackLayer = new Layer
  override def populateFigures() {
    super.populateFigures()
    boxDef.children foreach {
      _ match {
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
          new OpenPortDeclFigure(p, OpenBoxFigure.this).show()
        case _ ⇒
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
  //def clear { layer.removeAll } // FIXME
  override def useLocalCoordinates = true
  val inners = new LayeredPane
  inners.add(layer)
  inners.add(portsLayer)
  inners.add(connectionsLayer)
  inners.add(feedbackLayer)
  // inners.setSize(600,600) // TODO fix
  add(inners)
  setBorder(new LineBorder(5))

  override def update() {
    super.update()
    inners.setSize(this.getSize)

    populate()
  }
}