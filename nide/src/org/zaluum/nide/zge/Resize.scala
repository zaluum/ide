package org.zaluum.nide.zge
import org.zaluum.nide.compiler._
import scala.collection.mutable.Buffer

object Resize {
  private def minOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.min
  private def maxOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.max
  type PF = PartialFunction[Item, Rect]

  /* 1 - moure continguts de toResize a zerovec
   * 2 - moure germans de toResize per fer-li lloc
   * 3 - recursiu toResize amb parentnerec però tenint en compte 2!
   * */

  private def resizePF(toResize: OpenBoxFigure, newRect: Rect, f: PF): PF = {
    if (toResize.rect == newRect) f
    else {
      ensureSize(toResize.container,
        allocate(toResize, newRect,
          resizeInternal(toResize, newRect, f)))
    }
  }
  // returns movements to siblings to allocate newRect
  private def allocate(toResize: Item, newRect: Rect, f: PF): PF = {
    val others: Seq[Item] = toResize.container.boxes.filterNot(_ == toResize)
    val oldRect = f(toResize)
    assert(oldRect == toResize.rect)
    // fs not needed?
    val affected = others.filter { o ⇒ newRect.intersects(f(o)) }
    val leftD = minOrZero(affected.filter { f(_).leftOf(oldRect) }.map { newRect.left - f(_).right })
    val topD = minOrZero(affected.filter { f(_).aboveOf(oldRect) }.map { newRect.top - f(_).bottom })
    val rightD = maxOrZero(affected.filter { f(_).rightOf(oldRect) }.map { newRect.right - f(_).left })
    val bottomD = maxOrZero(affected.filter { f(_).belowOf(oldRect) }.map { newRect.bottom - f(_).top })
      def movement(o: Item) = {
        val dx = if (f(o).leftOf(oldRect)) leftD else if (f(o).rightOf(oldRect)) rightD else 0
        val dy = if (f(o).aboveOf(oldRect)) topD else if (f(o).belowOf(oldRect)) bottomD else 0
        f(o) + Vector2(dx, dy)
      }
    (others.map(o ⇒ (o -> movement(o))).toMap + (toResize -> newRect)) orElse (f)
  }
  private def ensureSize(container: ContainerItem, f: PF): PF = {
    container match {
      case o: OpenBoxFigure ⇒
        val contentsMinRect = container.boxes.map(f)
          .reduce(_.union(_))
          .growSize(container.borderVec)
        val parentMinRect = container.translateMineToParent_!(rectangleF(contentsMinRect))
        val parentNewRect = parentMinRect.union(container.rect)
        resizePF(o, parentNewRect, f)
      case _ ⇒ f
    }
  }
  private def resizeInternal(toResize: ContainerItem, newRect: Rect, f: PF): PF = {
    toResize match {
      case o: OpenBoxFigure ⇒
        val zeroVec = toResize.pos - newRect.pos
        toResize.boxes.map(b ⇒ (b: Item) -> (f(b) + zeroVec)).toMap.orElse(f)
      case _ ⇒ f
    }
  }
  def resize(o: OpenBoxFigure, newRect: Rect): (Transformer ⇒ TreePF) = {
    val pf = resizePF(o, newRect, new PF {
      def isDefinedAt(i: Item) = true
      def apply(i: Item) = i.rect
    })
    val valMap = o.viewer.deepChildren.collect {
      case i: ValDefItem if !(i.isInstanceOf[LabelItem]) ⇒ // XXX ugly 
        (i.valDef -> pf(i))
    }.toMap
    (e: Transformer) ⇒ {
      case v: ValDef if (valMap.contains(v)) ⇒
        val rect = valMap(v)
        v.copy(pos = rect.pos,
          size = v.size.map(_ ⇒ rect.dim),
          template = e.transformOption(v.template))
    }: TreePF
  }
}