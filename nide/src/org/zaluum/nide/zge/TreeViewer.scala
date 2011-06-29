package org.zaluum.nide.zge

import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.jdt.core.JavaCore
import org.zaluum.nide.eclipse.GraphicalEditor
import org.eclipse.swt.dnd.ByteArrayTransfer
import org.eclipse.swt.dnd.Transfer
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.FreeformLayer
import org.eclipse.draw2d.IFigure
import scala.collection.mutable.Buffer
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler._

class TreeViewer(parent: Composite, controller: Controller, editor: GraphicalEditor)
  extends ItemViewer(parent, controller) with ContainerItem with ClipboardViewer {
  /*MODEL*/
  def tree = controller.tree.asInstanceOf[BoxDef]
  def boxDef = tree
  def owner = null
  def viewer = this
  /*LAYERS*/
  val tool: TreeTool = new TreeTool(this)
  def zproject = controller.zproject
  def gotoMarker(l: Int) {
    controller.findPath(l) foreach { t ⇒
      selection.updateSelection(Set(t), false)
      refresh()
      focus
    }
  }
  // Viewer doesn't have any visual representation
  override def updateSize() {}
  val feed = null
  def hideme() {}
  def showme() {}
  def size = Dimension(0, 0)
  def pos = Point(0, 0)
  def container = this
  def myLayer = null
  val ports = Buffer[PortDeclFigure]()
  def updatePorts(changes: Map[Tree, Tree]) {
    ports.foreach { _.hide }
    ports.clear
    boxDef.children foreach {
      _ match {
        case p @ PortDef(name, typeName, dir, inPos, extPos) ⇒
          val f = new PortDeclFigure(p, TreeViewer.this)
          f.update()
          ports += f
        case _ ⇒
      }
    }
    ports.foreach { _.show }
  }
  override def dispose() {
    super.dispose()
  }
  import RichFigure._
  def remapSelection(m: PartialFunction[SelectionSubject, SelectionSubject]) {
    val mapper = m.orElse {
      //map LineSelectionSubject
      new PartialFunction[SelectionSubject, SelectionSubject] {
        def isDefinedAt(s: SelectionSubject): Boolean = {
          s match {
            case l: LineSelectionSubject ⇒
              if (m.isDefinedAt(l.c)) {
                val cd = m(l.c).asInstanceOf[ConnectionDef]
                Edge(cd).lines exists { _ == l.l }
              } else false
            case _ ⇒ false
          }
        }
        def apply(s: SelectionSubject): SelectionSubject = {
          val l = s.asInstanceOf[LineSelectionSubject]
          val cd = m(l.c).asInstanceOf[ConnectionDef]
          LineSelectionSubject(cd, l.l)
        }
      }
    };
    selection.refresh(mapper);
  }
  
  def itemToIType(i:Item) = {
    i match {
      case v:ValDefItem if (v.valSym.tpe!=NoSymbol) =>
        controller.zproject.jProject.findType(v.valSym.tpe.name.str)
      case _ => null
    }
  }
  def refresh() {
    updateContents(Map()) // FIXME
    selectedItems foreach { _.showFeedback() }
    selectedItems.headOption foreach { i => editor.setSelection(itemToIType(i)) }
    /*for (s <- selectedItems; ss <- s.selectionSubject) {
      println ("selected : " + ss)
      ss match {
        case t:Tree => 
          println ("symbol: " + t.symbol)
          println ("tpe: " + t.tpe )
        case _ =>
      }
    }*/
  }
  def selectedItems = this.deepChildren.collect {
    case i: Item if i.selectionSubject.isDefined && selection(i.selectionSubject.get) ⇒ i
  }.toSet
  def graphOf(b: BoxDef) = {
    if (boxDef == b) Some(graph)
    else {
      this.deepChildren.view.collect {
        case c: ContainerItem ⇒ c
      } filter { _.boxDef == b } map { _.graph } headOption
    }
  }
}
