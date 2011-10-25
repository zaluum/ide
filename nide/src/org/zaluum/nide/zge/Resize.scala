package org.zaluum.nide.zge
import org.zaluum.nide.compiler._
import scala.collection.mutable.Buffer

object Resize {
  private def minOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.min
  private def maxOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.max
  type PF = PartialFunction[Item, Rect]
  type CPF = PartialFunction[ValDef, Vector2]
  type F = (PF, CPF)
  //case class F(pf: PF, container: CPF)
  /* 1 - moure continguts de toResize a zerovec
   * 2 - moure germans de toResize per fer-li lloc
   * 3 - recursiu toResize amb parentnerec però tenint en compte 2!
   * */

  private def resizePF(toResize: OpenBoxFigure, newRect: Rect, changes: F): F = {
    if (toResize.rect == newRect) changes
    else {
      ensureSize(toResize.container,
        allocate(toResize, newRect,
          resizeInternal(toResize, newRect, changes)))
    }
  }
  // returns movements to siblings to allocate newRect
  private def allocate(toResize: Item, newRect: Rect, f: F): F = {
    val others: Seq[Item] = toResize.container.boxes.filterNot(_ == toResize)
    val oldRect = toResize.rect
    // fs not needed?
    val affected = others.filter { o ⇒ newRect.intersects(o.rect) }
    val leftD = minOrZero(affected.filter { _.rect.leftOf(oldRect) }.map { newRect.left - _.rect.right })
    val topD = minOrZero(affected.filter { _.rect.aboveOf(oldRect) }.map { newRect.top - _.rect.bottom })
    val rightD = maxOrZero(affected.filter { _.rect.rightOf(oldRect) }.map { newRect.right - _.rect.left })
    val bottomD = maxOrZero(affected.filter { _.rect.belowOf(oldRect) }.map { newRect.bottom - _.rect.top })
      def movement(o: Item) = {
        val dx = if (o.rect.leftOf(oldRect)) leftD else if (o.rect.rightOf(oldRect)) rightD else 0
        val dy = if (o.rect.aboveOf(oldRect)) topD else if (o.rect.belowOf(oldRect)) bottomD else 0
        o.rect + Vector2(dx, dy)
      }
    val (pf, cpf) = f
    ((others.map(o ⇒ (o -> movement(o))).toMap + (toResize -> newRect)) orElse pf,
      cpf)
  }
  private def ensureSize(container: ContainerItem, f: F): F = {
    container match {
      case o: OpenBoxFigure ⇒
        val contentsMinRect = container.boxes.map(f._1)
          .reduce(_.union(_))
          .growSize(container.borderVec)
        val parentMinRect = container.translateMineToParent_!(rectangleF(contentsMinRect))
        val parentNewRect = parentMinRect.union(container.rect)
        resizePF(o, parentNewRect, f)
      case _ ⇒ f
    }
  }
  // FIXME resize all blocks!
  private def resizeInternal(toResize: ContainerItem, newRect: Rect, f: F): F = {
    toResize match {
      case o: OpenBoxFigure ⇒
        val zeroVec = toResize.pos - newRect.pos
        val (pf, cpf) = f
        (
          toResize.boxes.map(b ⇒ (b: Item) -> (pf(b) + zeroVec)).toMap.orElse(pf),
          Map(o.valDef -> zeroVec) orElse (cpf))
      case _ ⇒ f
    }
  }
  def resize(o: OpenBoxFigure, newRect: Rect): (Transformer ⇒ TreePF) = {
    val (pf, cpf) = resizePF(o, newRect, (
      new PF {
        def isDefinedAt(i: Item) = true
        def apply(i: Item) = i.rect
      },
      new CPF {
        def isDefinedAt(i: ValDef) = true
        def apply(i: ValDef) = Vector2(0, 0)
      }))
    val valMap = o.viewer.deepChildren.collect {
      case i: ValDefItem if !(i.isInstanceOf[LabelItem]) ⇒ // XXX ugly 
        (i.valDef -> pf(i))
    }.toMap
    var vec = Vector2(0, 0)
    (e: Transformer) ⇒ {
      case v: ValDef if (valMap.contains(v)) ⇒
        val rect = valMap(v)
        vec = cpf(v)
        v.copy(pos = rect.pos,
          size = v.size.map(_ ⇒ rect.dim),
          template = e.transformOption(v.template))
      case c: ConnectionDef if (vec != Vector2(0, 0)) ⇒
        c.copy(e.transformOption(c.a), e.transformOption(c.b), points = c.points map { _ + vec })
    }: TreePF
  }
}