package org.zaluum.nide.zge

import javax.swing.JSlider
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.compiler._
import javax.swing.JButton
import javax.swing.JComponent
import java.awt.{ Graphics ⇒ AG }
import java.awt.image.BufferedImage
import org.eclipse.draw2d.{ Figure, Graphics }
import org.eclipse.swt.widgets.{ Composite, Display, Shell, Listener, Event }
import org.eclipse.swt.SWT
import scala.collection.mutable.Buffer

class SwingFigure(val container: Container, val box: ValDef, val component: JComponent) extends SimpleItem with ResizableFeedback {
  setOpaque(true)
  type T = ValDef
  def tree = box
  def size = box.guiSize getOrElse { Dimension(15, 15) }
  def pos = box.guiPos getOrElse { Point(0, 0) }
  def myLayer = container.layer
  override def paintFigure(g: Graphics) {
    val rect = getClientArea()
    component.setBounds(0, 0, rect.width, rect.height);
    component.doLayout
    val aimage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB)
    val ag = aimage.createGraphics
    component.paint(ag)
    val imageData = SWTUtils.convertAWTImageToSWT(aimage)
    val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(), imageData)
    g.drawImage(image, rect.x, rect.y)
    ag.dispose();
    image.dispose()
  }
}
class GuiViewer(parent: Composite, controller: Controller, val global: EclipseBoxClasspath)
  extends ItemViewer(parent, controller) with Container with ViewerResources {
  /*TOOLS*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.global)
  val helpers = Buffer[ShowHide]()
  /*MODEL*/
  def tree = controller.tree.asInstanceOf[BoxDef]
  def boxDef = tree
  def owner = global.root
  /*LAYERS*/
  def viewerResources = this
  val tool: Tool = new GuiTool(this);
  override def dispose() {
    super.dispose()
    imageFactory.reg.dispose
  }
  import RichFigure._
  def remapSelection(m: Map[Tree, Tree]) {
    selection.refresh(m);
  }

  def forName(str: String): Option[Class[_]] = {
    try { Some(global.classLoader.loadClass(str)) }
    catch { case e: Exception ⇒ e.printStackTrace; None }
  }

  def populate() {
    boxDef.children foreach {
      _ match {
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
          val sym = v.symbol.asInstanceOf[ValSymbol]
          val tpe = sym.tpe.asInstanceOf[BoxTypeSymbol]
          for (c <- tpe.visualClass; cl <- forName(c.str)) {
            helpers += new SwingFigure(GuiViewer.this, v,
              cl.newInstance.asInstanceOf[JComponent])
          }
        case _ ⇒
      }
    }
  }
  def refresh() {
    helpers.clear
    clear
    populate()
    helpers.foreach { _.show }
    selectedItems foreach { _.showFeedback() }
  }
  def selectedItems = this.deepChildren.collect { case i: TreeItem if selection(i.tree) ⇒ i }.toSet
  refresh()
}
class GuiTool(guiViewer: GuiViewer) extends ItemTool(guiViewer) {
  type C = Container
  override val resizing = new Resizing {
    override def command(newPos: Point, newSize: Dimension,t:Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize) if (v == t) ⇒
          ValDef(name, typeName, pos, size, Some(newPos), Some(newSize))
      }
    }
  }
  override val moving = new MovingItem {
    override def buttonUp {
      val positions = movables.map { item ⇒
        val oldLoc = item.getBounds.getLocation
        (item.tree.asInstanceOf[ValDef] -> (Point(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize) if (positions.contains(v)) ⇒
            ValDef(name, typeName, pos, size, Some(positions(v)), guiSize)
        }
      }
      controller.exec(command)
    }
  }
}
