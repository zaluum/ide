package org.zaluum.nide.zge

import scala.collection.mutable.Buffer
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.RectangleFigure
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.Dimension
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.TemplateExprType
import org.zaluum.nide.eclipse.GraphicalEditor
import org.eclipse.draw2d.Graphics
import org.eclipse.swt.events.KeyListener
import org.eclipse.swt.events.KeyEvent
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.BoxExprType

class GuiViewer(parent: Composite, controller: Controller, val editor: GraphicalEditor)
    extends ItemViewer(parent, controller) with ClipboardViewer {
  // TODO integrate eclipse commands
  canvas.addKeyListener(new KeyListener() {
    def keyPressed(e: KeyEvent) {
      if ((e.stateMask & SWT.CTRL) != 0) {
        e.keyCode match {
          case 'z' ⇒ controller.undo()
          case 'y' ⇒ controller.redo()
          case 'x' ⇒ viewer.tool.handleCut()
          case 'c' ⇒ viewer.tool.handleCopy()
          case 'v' ⇒ viewer.tool.handlePaste()
          case _   ⇒
        }
      } else {
        e.keyCode match {
          case SWT.DEL ⇒ viewer.tool.handleDel()
          case SWT.F5 ⇒
            controller.zproject.refreshClassLoader
            viewer.refresh()
          case _ ⇒
        }
      }
    }
    def keyReleased(e: KeyEvent) {}
  })
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
  def treeViewer = editor.viewer
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
  val backRect = new RectangleFigure() {
    override def fillShape(g: Graphics) {
      DotPainter.dotFill(g, getBounds, tool.gridSize, tool.gridSize)
    }
  }
  def updatePorts(changes: Map[Tree, Tree]) {}
  backRect.setForegroundColor(ColorConstants.lightBlue)
  backRect.setBackgroundColor(ColorConstants.white)
  background.add(backRect)
  override def updateContents(changes: Map[Tree, Tree]) {
    items.foreach { _.hide }
    items.clear()
      def updateBlock(b: Block) {
        b.valDefs foreach { v ⇒
          v.sym.tpe match {
            case Some(BoxExprType) if (v.sym.isVisual) ⇒
              val f = new SwingFigure(editor.viewer, GuiViewer.this, controller.zproject.classLoader)
              f.updateValDef(v)
              items += f
              f.show()
              v.labelGui foreach { l ⇒
                val lbl = new LabelItem(GuiViewer.this, gui = true)
                lbl.updateValDef(v)
                items += lbl
                lbl.show()
              }
            case Some(s: TemplateExprType) ⇒
              v.sym.blocks foreach { b ⇒ updateBlock(b.decl) }
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
