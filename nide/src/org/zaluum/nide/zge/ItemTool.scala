package org.zaluum.nide.zge

import org.zaluum.nide.compiler.EditTransformer
import draw2dConversions._
import org.eclipse.swt.graphics.Cursor
import org.eclipse.draw2d.{ Cursors, Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Point => EPoint, Rectangle }
import org.zaluum.nide.compiler.{  _ }
import scala.collection.JavaConversions._
import scala.reflect.Manifest._
import FigureHelper._

/**
 * Implements basic selecting, marquee and resizing of ItemFigures
 * @author frede
 *
 */
abstract class ItemTool(viewer: ItemViewer) extends LayeredTool(viewer) {
  def selecting : Selecting
  type C = ContainerItem
  state = selecting
  // SELECTING 
  abstract class Selecting extends ToolState {
    var beingSelected: Option[Item] = None
    var initDrag: Point = _
    var initContainer: C = _
    var filterDouble = false
    def enter() { state = this}

    def buttonDown {
      beingSelected = itemUnderMouse
      initDrag = currentMouseLocation
      initContainer = current
    }

    val handleTrack = new OverTrack[HandleRectangle] {
      def container = viewer.feedbackLayer
      override def onEnter(h: HandleRectangle) {
        super.onEnter(h)
        h.setXOR(true);
        viewer.setCursor(h.resizeCursor)
      }
      override def onExit(h: HandleRectangle) {
        super.onExit(h)
        h.setXOR(false);
        viewer.setCursor(null)
      }
    }
    def move {
      handleTrack.update()
    }
    
    def exit {}
    def abort {}
  }
  //// MOVE
  // SPECIAL move
  trait SpecialMove[A<:Item] {
    self : ToolState with DeltaMove with SingleContainer =>
    var fig : A = _
    def enter(initDrag:Point, initContainer:ContainerItem, fig: A) {
      this.fig = fig
      enterMoving(initDrag)
      enterSingle(initContainer)
      state=this
    }
    def clampDelta : Vector2
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { fig.moveFeed(snap (fig.pos + clampDelta)) }
    def abort() {
      fig.moveFeed(fig.pos)
      exit()
    }  }  
  // Direct edit
  object directEditing extends ToolState {
    var e: TextEditFigure = null
    var gui = false
    def enter(e: TextEditFigure, gui:Boolean=false) {
      state = this
      this.e = e;
      this.gui = gui
      e match {
        case d:DirectValFigure => 
          d.edit(editDirectParam(e.asInstanceOf[DirectValFigure], Name("setParam"), _), exit _)
        case l:LabelItem => 
          l.edit(editLabel(e.asInstanceOf[LabelItem],_), exit _)
        case _=> exit
      }
    }
    def editDirectParam(dvf : DirectValFigure, key: Name, value: String) {
      val valDef = dvf.valDef
      val c = valDef.params find { p ⇒ p.asInstanceOf[Param].key == key } match {
        case Some(par: Param) if (par.value != value) ⇒
          controller.exec(
            new EditTransformer() {
              val trans: PartialFunction[Tree, Tree] = {
                case p: Param if Some(p) == dvf.param ⇒ Param(p.key, value)
              }
            })
        case None ⇒
          controller.exec(
            new EditTransformer() {
              val trans: PartialFunction[Tree, Tree] = {
                case v: ValDef if v==valDef ⇒ v.copy(params = Param(key, value) :: transformTrees(v.params))
              }
            })
        case _ ⇒ // TODO exit?
      }
    }
    def editLabel(l:LabelItem,s:String) {
      controller.exec(
          new EditTransformer(){
            val trans : PartialFunction[Tree,Tree] = {
              case v:ValDef if (v==l.valDef) =>
                val oldl = if (gui) v.labelGui else v.label
                val lDesc = LabelDesc(s, oldl map {_.pos} getOrElse { Vector2(0,0) })
                if (gui)
                  v.copy(labelGui = Some(lDesc))
                else
                  v.copy(label = Some(lDesc))
            }
          })
    }
    def exit() { e.hideEdit(); viewer.focus; selecting.enter(); }
    def buttonDown() { exit() }
    def move() {}
    def buttonUp() {}
    def drag() {}
    override def menu() {}
    def abort() { exit() }
  }
  /// MARQUEE
  object marqueeing extends DeltaMove with SingleContainer {
    var absInit = Point(0,0)
    def enter(p: Point, initContainer: C) {
      import RichFigure._
      enterSingle(initContainer)
      enterMoving(p)
      absInit = initContainer.translateToViewport(p)
      state = this
      viewer.setCursor(Cursors.CROSS)
      viewer.showMarquee()
      move()
    }
    def exit() {
      viewer.setCursor(null)
      selecting.enter()
    }
    def abort() { exit() }
    def buttonDown {}
    def drag {}
    def buttonUp {
      viewer.hideMarquee()
      val r = new Rectangle(point(currentMouseLocation),point(initDrag))
      val touching = initContainer.shallowItems filter { i=> (r.touches(i.getBounds))  }
      val sel = for (i <- touching; s <- i.selectionSubject) yield s
      viewer.selection.updateSelection(sel.toSet, shift)
      viewer.refresh
      exit()
    }
    override def move { 
      viewer.moveMarquee(new Rectangle(point(absMouseLocation), point(absInit))) 
    } 
  }
  // RESIZING
  val resizing  = new Resizing
  class Resizing extends DeltaMove with SingleContainer {
    var handle: HandleRectangle = _
    def itf = handle.resizeItemFigure
    def enter(initDrag: Point, initContainer: C, handle: HandleRectangle) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      this.handle = handle
      state = this
    }
    def buttonUp {
      val newBounds = handle.deltaAdd(delta, itf.getBounds);
      val newPos = newBounds.getLocation
      
      val newSize = Geometry.maxDim(Dimension(newBounds.width, newBounds.height), Dimension(24, 24))
      // TODO snap
      itf match {
        case vd: ValDefItem =>
          controller.exec(command(newPos, newSize,vd.valDef))
        case _ => abort() 
      }
    }
    def command(newPos: Point, newSize: Dimension,t:Tree) = new EditTransformer {
      val trans: PartialFunction[Tree, Tree] = {
        case v:ValDef if (v == t) ⇒
          v.copy(size=Some(newSize), pos=newPos)
      }
    }
    def move() { itf.resizeDeltaFeed(delta, handle) }
    def abort() {
      itf.resizeDeltaFeed(Vector2(0, 0), handle)
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
  }

}
