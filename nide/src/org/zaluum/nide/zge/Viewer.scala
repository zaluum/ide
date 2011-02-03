package org.zaluum.nide.zge
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.model._
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.newcompiler._
class Viewer(parent: Composite, controller: Controller) extends AbstractViewer(parent, controller) {
  /*TOOLS*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.global)
  val palette = new Palette(this, parent.getShell, controller.global)
  var tool = new BoxTool(this)
  /*MODEL*/
  lazy val modelView = new TreeView(this,controller.global)

  def tree = controller.tree
  override def dispose() {
    super.dispose()
    imageFactory.reg.dispose
  }
}

class TreeView(override val viewer: Viewer, global:EclipseBoxClasspath) extends AbstractModelView(viewer) {
  object figureCreator extends Traverser(global.root) {
    override def traverse(tree:Tree) {
      super.traverse(tree)
      tree match {
         case EmptyTree ⇒
         case p@PortDef(name, typeName, in, inPos, extPos) ⇒
           new PortDeclFigure(p, TreeView.this)
         case v@ValDef(name, typeName,pos,guiSize) ⇒
           new ImageBoxFigure(v,TreeView.this) 
         case ConnectionDef(a, b) ⇒
         case _ =>
      }
    }
  }
  def tree = viewer.controller.tree
  def update() {
    viewer.clear()
    figureCreator.traverse(tree)
  }
  def gotoMarker(l: Location) {} // TODO
}
