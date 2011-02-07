package org.zaluum.nide.zge
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Composite
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
  
  def tree = viewer.controller.tree
  def update() {
    viewer.clear()
    // Create Figures
    new Traverser(global.root) {
      override def traverse(tree:Tree) {
        super.traverse(tree)
        tree match {
        case EmptyTree ⇒
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
          new PortDeclFigure(p, TreeView.this).show()
        case v@ValDef(name, typeName,pos,guiSize) ⇒
          new ImageBoxFigure(v,TreeView.this).show()
        case _ =>
        }
      }
    }.traverse(tree)
    // create connections (need to find figures positions)
    new Traverser(global.root) {
      override def traverse(tree:Tree) {
        super.traverse(tree)
        tree match {
        case c@ConnectionDef(a, b) ⇒
           new ConnectionFigure(c,TreeView.this).show()
        case _ =>
        }
      }
    }.traverse(tree)
  }
  // finders
  import scala.collection.JavaConversions._
  private def portFigures = viewer.portsLayer.getChildren.collect { case p:PortFigure => p } 
  def findPortFigure(boxName:Name,portName:Name, in:Boolean) : Option[PortFigure] =
    portFigures find {
      p=>p.valSym match {
        case Some(valSym) => (valSym.name==boxName && p.sym.name==portName && p.in == in)
        case None => false
      }
    }
  def findPortFigure(portName:Name, in:Boolean) : Option[PortFigure] = 
    portFigures find { p =>  p.valSym.isEmpty && p.sym.name == portName && p.in == in }
    
  def gotoMarker(l: Location) {} // TODO
}
