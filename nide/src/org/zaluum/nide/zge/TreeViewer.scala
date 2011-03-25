package org.zaluum.nide.zge

import org.eclipse.draw2d.IFigure
import scala.collection.mutable.Buffer
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.eclipse.EclipseBoxClasspath
import org.zaluum.nide.compiler._

trait ViewerResources { // XXX rename
  def imageFactory: ImageFactory
}
class TreeViewer(parent: Composite, controller: Controller, val global: EclipseBoxClasspath)
  extends ItemViewer(parent, controller) with BoxDefContainer with ViewerResources {
  /*TOOLS*/
  lazy val imageFactory = new ImageFactory(parent.getDisplay, controller.global)
  val palette = new Palette(this, parent.getShell, controller.global)
  val helpers = Buffer[ShowHide]()
  /*MODEL*/
  def tree = controller.tree.asInstanceOf[BoxDef]
  def boxDef = tree
  def owner = global.root
  def viewer = this
  /*LAYERS*/
  def viewerResources = this
  val tool: TreeTool = new TreeTool(this)
  def gotoMarker(l: Location) {
    // IDEA look at controllers save mark and then transform the selection to get the current blame node
    tree.findPath(l.path) foreach { t ⇒
      selection.deselectAll
      selection.select(t)
      refresh()
      focus
    }
  }
  override def createFigures() {
    super.createFigures()
    boxDef.children foreach {
      _ match {
        case p@PortDef(name, typeName, dir, inPos, extPos) ⇒
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
  def remapSelection(m: PartialFunction[SelectionSubject, SelectionSubject]) {
    val mapper = m.orElse{ 
      //map LineSelectionSubject
      new PartialFunction[SelectionSubject,SelectionSubject] { 
        def isDefinedAt(s:SelectionSubject) : Boolean = {
            s match {
              case l:LineSelectionSubject =>
                if (m.isDefinedAt(l.c)) {
                  val cd = m(l.c).asInstanceOf[ConnectionDef]
                  Edge(cd).lines exists {_ == l.l }
                }else false
              case _ => false
            }
        }
        def apply(s:SelectionSubject) : SelectionSubject = {
          val l = s.asInstanceOf[LineSelectionSubject]
          val cd = m(l.c).asInstanceOf[ConnectionDef]
          LineSelectionSubject(cd,l.l)
        }
      }
    };
    selection.refresh(mapper);
  }
  def refresh() {
    helpers.clear
    clear
    createFigures
    helpers.foreach { _.show }
    val cf = createConnectionFigures
    cf.foreach {_.show }
    println(cf)
    helpers ++=cf
    selectedItems foreach { _.showFeedback() }
  }
  def selectedItems = this.deepChildren.collect {
    case i: Item if i.selectionSubject.isDefined && selection(i.selectionSubject.get) ⇒ i
  }.toSet
}
