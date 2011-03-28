package org.zaluum.nide.zge

import javax.swing.JComponent
import org.eclipse.swt.widgets.{Composite, Display}
import org.zaluum.nide.compiler._
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import scala.collection.mutable.Buffer

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
  def viewer = this
  def viewerResources = this
  val tool: Tool = new GuiTool(this);
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

  def populate() {
    def populateBoxDef(b: BoxDef) {
      b.children foreach {
        _ match {
          case v: ValDef ⇒
            val sym = v.symbol.asInstanceOf[ValSymbol]
            val tpe = sym.tpe.asInstanceOf[BoxTypeSymbol]
            if (!tpe.isLocal) {
              for (c ← tpe.visualClass; cl ← forName(c.str)) {
                helpers += new SwingFigure(GuiViewer.this, v,
                  cl.newInstance.asInstanceOf[JComponent])
              }
            }
          case childBox: BoxDef ⇒ populateBoxDef(childBox)
          case _ ⇒
        }
      }
    }
    populateBoxDef(boxDef)
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
