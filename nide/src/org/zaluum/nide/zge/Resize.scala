package org.zaluum.nide.zge
import org.zaluum.nide.compiler._

object Resize {
  private def minOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.min
  private def maxOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.max
  def resize[R <: Rect](oldRect: Rect, newRect: Rect, others: Iterable[R]): Map[R, Vector2] = {
    val affected = others.filter { o ⇒ newRect.intersects(o) }
    val leftD = minOrZero(affected.filter { _.leftOf(oldRect) }.map { newRect.left - _.right })
    val topD = minOrZero(affected.filter { _.aboveOf(oldRect) }.map { newRect.top - _.bottom })
    val rightD = maxOrZero(affected.filter { _.rightOf(oldRect) }.map { newRect.right - _.left })
    val bottomD = maxOrZero(affected.filter { _.belowOf(oldRect) }.map { newRect.bottom - _.top })
      def movement(o: R) = {
        val dx = if (o.leftOf(oldRect)) leftD else if (o.rightOf(oldRect)) rightD else 0
        val dy = if (o.aboveOf(oldRect)) topD else if (o.belowOf(oldRect)) bottomD else 0
        Vector2(dx, dy)
      }
    others.map(o ⇒ (o -> movement(o))).toMap
  }

  def resize(toResize: ContainerItem, newRect: Rect): (Transformer ⇒ TreePF) = {
    toResize match {
      case o: OpenBoxFigure ⇒
        val others = toResize.container.boxes - o
        val m = resize(toResize, newRect, others)
        val treeResize = m.map { case (v, k) ⇒ v.valDef -> k }
          def trans(e: Transformer): TreePF = {
            case v: ValDef if (v == o.valDef) ⇒
              v.copy(size = Some(newRect.rectSize), pos = newRect.rectPos, template = e.transformOption(v.template))
            case v: ValDef if (treeResize.contains(v)) ⇒
              v.copy(pos = v.pos + treeResize(v), template = e.transformOption(v.template))
          }
        val contentsNewRect = others.map { o ⇒
          val newPos = o.pos + m(o)
          RectInstance(newPos.x, newPos.y, o.w, o.h)
        }.fold(newRect)(_.union(_))
        val parent = toResize.container
        val parentMinRect = parent.translateMineToParent_!(rectangleF(contentsNewRect))
        val parentNewRect = parentMinRect.union(parent)
        // TODO stop early
        (e: Transformer) ⇒ trans(e) orElse resize(o.container, parentNewRect)(e)
      case _ ⇒ (e: Transformer) ⇒ Map.empty

    }

  }
}