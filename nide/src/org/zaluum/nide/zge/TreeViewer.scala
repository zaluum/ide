package org.zaluum.nide.zge

import scala.collection.mutable.Buffer
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.FreeformLayer
import org.eclipse.draw2d.Label
import org.eclipse.draw2d.ScalableFreeformLayeredPane
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler._
import org.zaluum.nide.eclipse.GraphicalEditor
import org.zaluum.nide.Activator
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.utils.EclipseUtils
import org.zaluum.nide.compiler.SymbolTree

class TreeViewer(parent: Composite, controller: Controller, val editor: GraphicalEditor)
    extends ItemViewer(parent, controller) with ClipboardViewer {
  /*MODEL*/
  def tree = controller.tree
  def boxDef = tree
  def block = boxDef.template.blocks.head
  /*LAYERS*/
  val tool: TreeTool = new TreeTool(this)
  def zproject = controller.zproject
  def gotoMarker(l: Int) {
    controller.findPath(l) foreach { t ⇒
      selection.updateSelection(Set(t), false)
      redraw()
      focus
    }
  }
  def onFocus { editor.showViews() }
  def onResize { redraw() } // FIXME this OnResize triggers when a tooltip expands the canvas
  // Viewer doesn't have any visual representation
  override def updateSize() {}
  val feed = null
  def size = Dimension(0, 0)
  def pos = Point(0, 0)
  def container = this
  def myLayer = null
  val ports = Buffer[PortDeclFigure]()
  /*Selection*/
  val selection = new SelectionManager[SelectionSubject] {
    def isContained(a: SelectionSubject, b: SelectionSubject) =
      b.selectedTree.deepContains(a.selectedTree)
  }
  def updatePorts(changes: UpdatePF) {
    ports.foreach { _.destroy() }
    ports.clear
    symbol.template.thisVal.portSides foreach { pside ⇒
      pside.pi.portSymbol match {
        case Some(ps) ⇒
          ps.decl match {
            case pd: PortDef ⇒
              val f = new PortDeclFigure(pd, pside, TreeViewer.this)
              f.update()
              ports += f
            case _ ⇒
          }
        case _ ⇒
      }
    }
    updateProperties()
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
  def updateProperties() {
    properties = List(
      new InitMethodProperty(controller.tree, controller),
      new InitMethodClassProperty(controller.tree, controller),
      new ConstructorDeclProperty(controller.tree, controller))
  }
  val emptyLabel = new Label("Empty File. Start by dropping items from the palette...")
  emptyLabel.setForegroundColor(ColorConstants.lightGray)
  emptyLabel.setFont(Activator.getDefault.directEditFont)
  def showEmptyLabel() = {
    val x = getBounds.width / 2
    val y = getBounds.height / 2
    val d = emptyLabel.getPreferredSize();
    val dx = d.width / 2
    val dy = d.height / 2
    emptyLabel.setBounds(new Rectangle(x - dx, y - dy, d.width, d.height))
    this.feedbackLayer.add(emptyLabel)
  }
  def hideEmptyLabel() = {
    if (feedbackLayer.getChildren.contains(emptyLabel))
      feedbackLayer.remove(emptyLabel)
  }
  def deepChildrenWithoutLayers = this.deepChildren.filter {
    _ match {
      case _: FreeformLayer               ⇒ false
      case _: ScalableFreeformLayeredPane ⇒ false
      case _                              ⇒ true
    }
  }
  def highLight(b: Boolean) {
    setBackgroundColor(if (b) ColorConstants.lightGray else ColorConstants.white)
  }

  def refresh() {
    /*this.deepChildren foreach {
      _ match {
        case i: Item ⇒ i.hideFeedback()
        case _       ⇒
      }
    }*/
    hideEmptyLabel()
    updateContents(Map()) // FIXME
    if (deepChildrenWithoutLayers.isEmpty) showEmptyLabel()
    redraw()
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
  def graphOf(b: Block) = {
    if (block == b) Some(graph)
    else {
      this.deepChildren.view.collect {
        case c: ContainerItem ⇒ c
      } filter { _.block == b } map { _.graph } headOption
    }
  }
  refresh()
}
