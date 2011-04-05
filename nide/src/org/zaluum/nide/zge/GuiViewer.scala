package org.zaluum.nide.zge

import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.FreeformLayer
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.RectangleFigure
import javax.swing.JComponent
import org.eclipse.swt.widgets.{Composite, Display}
import org.eclipse.swt.events._
import org.eclipse.swt.SWT

import org.zaluum.nide.compiler._
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import scala.collection.mutable.Buffer
import org.eclipse.draw2d.{IFigure,LayoutListener}
class GuiViewer(parent: Composite, controller: Controller, val global: EclipseBoxClasspath)
  extends ItemViewer(parent, controller) with ContainerItem with ViewerResources {
  /*TOOLS*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.global)
  val helpers = Buffer[Item]()
  val feed = null
  def pos = Point(0,0)
  def container = null
  def myLayer = null
  /*MODEL*/
  def tree = controller.tree.asInstanceOf[BoxDef]
  def boxDef = tree
  def owner = global.root
  background.setForegroundColor(ColorConstants.white)
  background.setBackgroundColor(ColorConstants.lightGray)
  /*LAYERS*/
  def viewer = this
  def viewerResources = this
  val tool: Tool = new GuiTool(this);
  var buttonDown : Boolean =true
  override def dispose() {
    super.dispose()
    imageFactory.reg.dispose
  }
  import RichFigure._
  def remapSelection(m: PartialFunction[SelectionSubject, SelectionSubject]) {
    selection.refresh(m);
  }

  def forName(str: String): Option[Class[_]] = {
    try { Some(global.classLoader.loadClass(str)) }
    catch { case e: Exception ⇒ e.printStackTrace; None }
  }
  val backRect = new RectangleFigure()  {
    override def fillShape(g:Graphics) {
      DotPainter.dotFill(g,getBounds)
    }
  }
  def updatePorts(changes:Map[Tree,Tree]){}
  backRect.setForegroundColor(ColorConstants.red)
  backRect.setBackgroundColor(ColorConstants.white)
  def populate() {
    def populateBoxDef(b: BoxDef) {
      b.children foreach {
        _ match {
          case v: ValDef ⇒
            val sym = v.symbol.asInstanceOf[ValSymbol]
            val tpe = sym.tpe.asInstanceOf[BoxTypeSymbol]
            if (!tpe.isLocal) {
              /*for (c ← tpe.visualClass; cl ← forName(c.str)) {
                helpers += new SwingFigure(GuiViewer.this, v,
                  cl.newInstance.asInstanceOf[JComponent])
              }*/
            }
          case childBox: BoxDef ⇒ populateBoxDef(childBox)
          case _ ⇒
        }
      }
    }
    populateBoxDef(boxDef)
    background.add(backRect)
  }
  def size = boxDef.guiSize.getOrElse(Dimension(200,200))
  def refresh() {
    helpers.foreach {_.hide }
    helpers.clear
    populate()
    val Dimension(w,h) = size
    backRect.setBounds(new Rectangle(0,0,w,h))
    helpers.foreach { _.show }
    selectedItems foreach { _.showFeedback() }
  }
  def selectedItems = Set() //FIXME this.deepChildren.collect { case i: Item if selection(i.tree) ⇒ i }.toSet
  shell.setSize(size.w + 30, size.h +40)
  refresh(); 
 
}
