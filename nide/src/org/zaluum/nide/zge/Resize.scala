package org.zaluum.nide.zge
import org.zaluum.nide.compiler.Vector2
import org.zaluum.nide.compiler.Rect

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
}