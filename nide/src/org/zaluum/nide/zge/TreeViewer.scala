package org.zaluum.nide.zge

import org.eclipse.draw2d.IFigure
import scala.collection.mutable.Buffer
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.model._
import org.zaluum.nide.newcompiler._
class TreeViewer(parent: Composite, controller: Controller, val global: EclipseBoxClasspath)
  extends ItemViewer(parent, controller) with BoxDefContainer {
  /*TOOLS*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.global)
  val palette = new Palette(this, parent.getShell, controller.global)
  val helpers = Buffer[ShowHide]()
  /*MODEL*/
  def tree = controller.tree.asInstanceOf[BoxDef]
  def boxDef = tree
  def owner = global.root
  /*LAYERS*/
  def viewer = this
  val tool: TreeTool = new TreeTool(this)
  def gotoMarker(l: Location) {} // TODO
  override def populateFigures() {
    super.populateFigures()
    boxDef.children foreach {
      _ match {
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
          helpers += new PortDeclFigure(p, TreeViewer.this)
        case _ ⇒
      }
    }
  }
  override def dispose() {
    super.dispose()
    imageFactory.reg.dispose
  }
  import RichFigure._
  def remapSelection(m : Map[Tree,Tree]){
    selection.refresh(m);
  }
  def refresh() {
    helpers.clear
    clear
    populate()
    helpers.foreach{_.show}
    this.deepChildren foreach  { _ match {
      case i : Item if selection(i.tree)=> i.showFeedback();
      case _ => 
      }
    }
  }
  def selectedItems = this.deepChildren.collect { case i:Item if selection(i.tree) => i}.toSet 
}
