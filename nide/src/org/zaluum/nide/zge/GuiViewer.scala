package org.zaluum.nide.zge

import scala.collection.mutable.Buffer
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.TemplateExprType
import org.zaluum.nide.eclipse.GraphicalEditor

class GuiViewer(parent: Composite, controller: Controller, editor: GraphicalEditor)
    extends ItemViewer(parent, controller) {
  /*TOOLS*/
  def zproject = controller.zproject
  val items = Buffer[Item]()
  val feed = null
  def pos = Point(0, 0)
  def container = null
  def myLayer = null
  /*MODEL*/
  def tree = controller.tree
  def boxDef = tree
  def block = boxDef.template.blocks.head
  def owner = null //global.root
  background.setForegroundColor(ColorConstants.white)
  background.setBackgroundColor(ColorConstants.lightGray)
  /*LAYERS*/
  def viewer = this
  def viewerResources = this
  val tool: Tool = new GuiTool(this);
  var buttonDown: Boolean = true
  override def dispose() {
    super.dispose()
  }
  def onFocus {}
  def onResize {}
  import RichFigure._
  def remapSelection(m: PartialFunction[SelectionSubject, SelectionSubject]) {
    selection.refresh(m);
  }
  val backRect = new RectangleFigure() /*{
    /*override def fillShape(g:Graphics) {
      DotPainter.dotFill(g,getBounds)
    }*/
  }*/
  def updatePorts(changes: Map[Tree, Tree]) {}
  backRect.setForegroundColor(ColorConstants.red)
  backRect.setBackgroundColor(ColorConstants.white)
  background.add(backRect)
  override def updateContents(changes: Map[Tree, Tree]) {
    items.foreach { _.hide }
    items.clear()
      def updateBlock(b: Block) {
        b.valDefs foreach { v ⇒
          v.tpe match {
            case bs: BoxTypeSymbol if (bs.isVisual) ⇒
              val f = new SwingFigure(GuiViewer.this, controller.zproject.classLoader)
              f.updateValDef(v)
              items += f
              f.show()
              v.labelGui foreach { l ⇒
                val lbl = new LabelItem(GuiViewer.this, gui = true)
                lbl.updateValDef(v)
                items += lbl
                lbl.show()
              }
            case s: TemplateExprType ⇒
              v.sym.blocks foreach { b ⇒ updateBlock(b.tdecl) }
            case _ ⇒
          }
        }
      }
    val Dimension(w, h) = size
    backRect.setBounds(new Rectangle(0, 0, w, h))
    updateBlock(block)
  }
  def size = boxDef.guiSize.getOrElse(Dimension(200, 200))
  def refresh() {
    updateContents(Map())
    selectedItems foreach { _.showFeedback() }
    editor.setSelection(selectedItems.headOption)
  }
  def selectedItems = this.deepChildren.collect {
    case i: Item if i.selectionSubject.isDefined && selection(i.selectionSubject.get) ⇒ i
  }.toSet
  shell.setSize(size.w + 30, size.h + 40)
  refresh();

}
