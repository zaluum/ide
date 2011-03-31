package org.zaluum.nide.zge

import org.eclipse.draw2d.Viewport
import org.eclipse.draw2d.Layer
import org.zaluum.nide.compiler.SelectionSubject
import draw2dConversions._
import org.eclipse.draw2d.{ Figure, IFigure }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.zaluum.nide.compiler.{Point ⇒ MPoint,_}
import scala.collection.mutable.Buffer

object draw2dConversions {
  def point(p: MPoint): EPoint = new EPoint(p.x, p.y)
  def dimension(d: Dimension): EDimension = new EDimension(d.w, d.h)
  implicit def rpoint(p: EPoint): MPoint = MPoint(p.x, p.y)
  implicit def rdimension(d: EDimension): Dimension = Dimension(d.width, d.height)
}
object FigureHelper {
  import scala.collection.JavaConversions._
  def isOrHas(item: IFigure, lookingFor: IFigure): Boolean = {
    if (item eq lookingFor) true
    else {
      val children = item.getChildren.asInstanceOf[java.util.List[IFigure]]
      children.exists { isOrHas(_, lookingFor) }
    }
  }
}
object RichFigure {
  implicit def richFigure(f: IFigure) = new RichFigure(f)
}
import RichFigure._
class RichFigure(container: IFigure) {
  import scala.collection.JavaConversions._
  def immediateChildren = container.getChildren.asInstanceOf[java.util.List[IFigure]].toList
  def deepChildren : List[IFigure] = {
    val deepChildren:List[IFigure] = immediateChildren.flatMap { _.deepChildren }.toList
    immediateChildren ++ deepChildren 
  }
  def translateFromViewport(p: MPoint) : MPoint = translateFromViewport(point(p))
  def translateFromViewport(p: EPoint): EPoint = {
    if (container.isInstanceOf[Viewport]) p.getCopy
    else {
      val ep = container.getParent.translateFromViewport(p)
      container.translateFromParent(ep)
      ep
    }
  }
  def deepChildrenNear(abs:EPoint, radius : Double) : List[(IFigure,Double)] = {
    def distance(f:IFigure) : Double = f.getBounds.getCenter.getDistance( f.translateFromViewport(abs))
    val v = for (c <- deepChildren.view; val d = distance(c); if (d<radius)) yield (c,d)
    v.toList
  }
  def findDeepAt[A](internalCoords: EPoint, deep: Int = 0, debug: Boolean = false)(partial: PartialFunction[IFigure, A]): Option[A] = {
    // bounds in parent coordinates
    // client area in relative coordinates 
    val spaces = new String(Array.fill(deep)(' '))
    def println2(str: String) = { if (debug) println(spaces + str) }
    var candidate: Option[A] = None
    val parentCoords = internalCoords.getCopy
    container.translateToParent(parentCoords)
    println2("findDeep " + container + " " + parentCoords + "bounds " + container.getBounds + " visible=" + container.isVisible + "opaque=" + container.isOpaque)
    if (container.isVisible && container.containsPoint(parentCoords)) {
      candidate = partial.lift(container) 
      println2("candidate = " + candidate)
      println2("contains point. Client area= " + container.getClientArea + " relative point=" + internalCoords)
      if (container.getClientArea.contains(internalCoords)) {
        println2("checking children")
        // search children 
        val list = container.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
        for (c ← list.reverse) {
          val childCoords = internalCoords.getCopy
          c.translateFromParent(childCoords)
          println2("checking child " + c + " with coordinates " + childCoords)
          c.findDeepAt(childCoords, deep + 1, debug)(partial) match {
            case Some(cc) ⇒
              println2("child found=" + cc)
              return Some(cc)
            case None ⇒
              println2("no child found")
          }
        }
      }
    }
    println2("returning candidate=" + candidate)
    return candidate
  }

  def findDeepContainerAt[A](internalCoords: EPoint)(partial: PartialFunction[IFigure, A]): Option[A] = {
    var candidate: Option[A] = None
    if (container.isVisible && container.getClientArea.contains(internalCoords)) {
      candidate = partial.lift(container)
      // search children 
      val list = container.getChildren.toBuffer.asInstanceOf[Buffer[IFigure]]
      for (c ← list.reverse) {
        val childInternalCoords = internalCoords.getCopy
        c.translateFromParent(childInternalCoords)
        c.findDeepContainerAt(childInternalCoords)(partial) match {
          case Some(cc) ⇒ return Some(cc)
          case None ⇒
        }
      }
    }
    return candidate
  }
}
trait Transparent extends Figure {
  import scala.collection.JavaConversions._
  override def containsPoint(x: Int, y: Int) = {
    val pt = new EPoint(x, y)
    translateFromParent(pt);
    if (getClientArea.contains(pt)) {
      getChildren.asInstanceOf[java.util.List[IFigure]].reverse exists (_.containsPoint(pt.x, pt.y))
    } else {
      getBounds.contains(x, y)
    }
  }
}

trait Item extends Figure with Feedback with ShowHide {
  def selectionSubject : Option[SelectionSubject] = None 
  def container :Container
  def moveFeed(loc: MPoint) 
  def moveDeltaFeed(delta: Vector2) 
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) 
}
trait Feedback {
  def showFeedback()
  def hideFeedback()
}
trait ShowHide {
  def show()
  def hide()
}
trait TreeItem extends Item {
  type T <: Tree
  def tree :T  
  override def selectionSubject = Some(tree)
}
trait SymbolItem extends Item {
  type S <: Symbol
  def sym : S
}
trait SimpleShowHide extends ShowHide{
  val helpers = Buffer[ShowHide]()
  def updateSize() 
  def populateFigures()
  def newConnectionFigures : Set[Item] 
  def show() {
    updateSize()
    helpers.clear
    populateFigures
    helpers.foreach { _.show() }
    val cf = newConnectionFigures
    cf foreach { _.show() }
    helpers ++= cf
    showme()
  }
  def showme()
  def hideme()
  def hide() {
    hideme()
    helpers.foreach { _.hide() }
  }
}
trait SimpleItem extends Item with SimpleShowHide{
  def myLayer: Figure
  def pos: MPoint
  def size: Dimension
  val feed: ItemFeedbackFigure
  def showFeedback() {
    container.feedbackLayer.add(feed)
  }
  def hideFeedback() {
    if (container.feedbackLayer.getChildren.contains(feed))
      container.feedbackLayer.remove(feed)
  }
  def updateSize() = {
    val rect = new Rectangle(pos.x, pos.y, size.w, size.h)
    setBounds(rect)
    feed.setInnerBounds(rect)    
  }
  def showme = myLayer.add(this)
  def hideme = {
    if (myLayer.getChildren.contains(this)) myLayer.remove(this)
  }
  override def hide() {
    super.hide()
    hideFeedback()
  }
  def moveFeed(loc: MPoint) {
    feed.setInnerLocation(point(loc))
  }
  def moveDeltaFeed(delta: Vector2) {
    val loc = pos + delta
    feed.setInnerLocation(point(loc))
  }
  def resizeDeltaFeed(delta: Vector2, handle: HandleRectangle) {
    feed.setInnerBounds(handle.deltaAdd(delta, getBounds))
  }
  def newConnectionFigures = Set()
}
trait RectFeedback extends Item {
  val feed = new ItemFeedbackFigure(container)
}

trait ResizableFeedback extends RectFeedback {
  override val feed: ResizeItemFeedbackFigure = new ResizeItemFeedbackFigure(this, container)
}
