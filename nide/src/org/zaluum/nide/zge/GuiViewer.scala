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
import scala.collection.mutable.Buffer
import org.eclipse.draw2d.{IFigure,LayoutListener}

class GuiViewer(parent: Composite, controller: Controller)
  extends ItemViewer(parent, controller) with ContainerItem {
  /*TOOLS*/
  def zproject = controller.zproject
  val items = Buffer[Item]()
  val feed = null
  def pos = Point(0,0)
  def container = null
  def myLayer = null
  /*MODEL*/
  def tree = controller.tree.asInstanceOf[BoxDef]
  def boxDef = tree
  def owner = null//global.root
  background.setForegroundColor(ColorConstants.white)
  background.setBackgroundColor(ColorConstants.lightGray)
  /*LAYERS*/
  def viewer = this
  def viewerResources = this
  val tool: Tool = new GuiTool(this);
  var buttonDown : Boolean =true
  override def dispose() {
    super.dispose()
  }
  import RichFigure._
  def remapSelection(m: PartialFunction[SelectionSubject, SelectionSubject]) {
    selection.refresh(m);
  }
  val backRect = new RectangleFigure()  /*{
    /*override def fillShape(g:Graphics) {
      DotPainter.dotFill(g,getBounds)
    }*/
  }*/
  def updatePorts(changes:Map[Tree,Tree]){}
  backRect.setForegroundColor(ColorConstants.red)
  backRect.setBackgroundColor(ColorConstants.white)
  background.add(backRect)
  override def updateContents(changes:Map[Tree,Tree]) {
    items.foreach {_.hide}
    items.clear()
    def updateBoxDef(b: BoxDef) {
      b.children foreach {
        _ match {
          case v: ValDef ⇒
            val sym = v.symbol.asInstanceOf[ValSymbol]
            val tpe = sym.tpe.asInstanceOf[BoxTypeSymbol]
            if (!tpe.isLocal && tpe.visualClass.isDefined) {
                val f = new SwingFigure(GuiViewer.this,controller.zproject.classLoader)
                f.updateValDef(v)
                items += f
                f.show()
                v.labelGui foreach { l =>
                  val lbl = new LabelItem(GuiViewer.this, gui=true)
                  lbl.updateValDef(v)
                  items += lbl
                  lbl.show()
                }
            }
          
          case childBox: BoxDef ⇒ updateBoxDef(childBox)
          case _ ⇒
        }
      }
    }
    val Dimension(w,h) = size
    backRect.setBounds(new Rectangle(0,0,w,h))
    updateBoxDef(boxDef)
  }
  def size = boxDef.guiSize.getOrElse(Dimension(200,200))
  def refresh() {
    updateContents(Map())
    selectedItems foreach { _.showFeedback() }
  }
  def selectedItems = this.deepChildren.collect {
    case i: Item if i.selectionSubject.isDefined && selection(i.selectionSubject.get) ⇒ i
  }.toSet
  shell.setSize(size.w + 30, size.h +40)
  refresh(); 
 
}
