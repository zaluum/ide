package org.zaluum.nide.zge
import org.zaluum.nide.compiler._

object Resize {
  private def minOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.min
  private def maxOrZero(t: TraversableOnce[Int]): Int = if (t.isEmpty) 0 else t.max
  type PF = PartialFunction[ValDef, Rect]
  type CPF = PartialFunction[ValDef, Vector2]
  type F = (PF, CPF)
  //case class F(pf: PF, container: CPF)
  /* 1 - moure continguts de toResize a zerovec
   * 2 - moure germans de toResize per fer-li lloc
   * 3 - recursiu toResize amb parentnerec però tenint en compte 2!
   * */

  private def resizePF(toResize: OpenBoxFigure, newRect: Rect, changes: F): F = {
    val min = minSize(toResize, changes)
    val fixed = newRect union min
    if (toResize.rect == fixed) changes
    else {
      ensureSize(toResize.container,
        allocate(toResize, fixed,
          resizeInternal(toResize, fixed, changes)))
    }
  }
  // returns movements to siblings to allocate newRect
  private def allocate(toResize: ValDefItem, newRect: Rect, f: F): F = {
    val others: List[ValDefItem] = toResize.container.boxes.toList.filterNot(_ == toResize)
    val oldRect = toResize.rect
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
    val othersMove = others.map(o ⇒ (o.valDef -> movement(o))).toMap
    ((othersMove + (toResize.valDef -> newRect)) orElse pf,
      cpf)

  }
  private def minSize(container: ContainerItem, f: F): Rect = {
    container match {
      case o: OpenBoxFigure ⇒
        // TODO cleanup constants
        val (pf, cpf) = f
        val portsY = o.portDecls map (_.intPort.pos.y + cpf(o.valDef).y)
        val border = container.borderVec + Vector2(8, 2)
        val rects: Seq[Rect] = container.boxes.map(i ⇒ pf(i.valDef))
        val portMinRect: Rect = if (portsY.size == 0)
          Rect.empty
        else {
          val min = portsY.min
          Rect(0, min - border.y, 0, portsY.max + 15 - min + border.y * 2)
        }
        val boxesMinRect = if (rects.size == 0)
          Rect.empty
        else
          rects.reduce(_.union(_))
            .growSize(border * 2) + border.negate
        val contentsMinRect = boxesMinRect union portMinRect
        val parentMinRect = container.translateMineToParent_!(rectangleF(contentsMinRect))
        parentMinRect
      case _ ⇒ Rect.empty
    }

  }
  private def ensureSize(container: ContainerItem, f: F): F = {
    container match {
      case o: OpenBoxFigure ⇒
        val parentNewRect = minSize(container, f).union(container.rect)
        resizePF(o, parentNewRect, f)
      case _ ⇒ f
    }
  }
  private def resizeInternal(toResize: ContainerItem, newRect: Rect, f: F): F = {
    toResize match {
      case o: OpenBoxFigure ⇒
        val zeroVec = toResize.pos - newRect.pos
        val (pf, cpf) = f
        val vals = o.valDef.template.get.blocks.flatMap(_.valDefs)
        (vals.map(v ⇒ v -> (pf(v) + zeroVec)).toMap.orElse(pf),
          Map(o.valDef -> zeroVec) orElse (cpf))
      case _ ⇒ f
    }
  }
  def resize(o: OpenBoxFigure, newRect: Rect): (Transformer ⇒ TreePF) = {
    val (pf, cpf) = resizePF(o, newRect, (
      new PF {
        def isDefinedAt(v: ValDef) = true
        def apply(v: ValDef): Rect = (v.pos, v.size.getOrElse(Dimension(0, 0)))
      },
      new CPF {
        def isDefinedAt(i: ValDef) = true
        def apply(i: ValDef) = Vector2(0, 0)
      }))
    var vec = Vector2(0, 0)
    (e: Transformer) ⇒ {
      case v: ValDef if (pf.isDefinedAt(v)) ⇒
        val rect = pf(v)
        val oldVec = vec
        vec = cpf(v)
        val res = v.copy(pos = rect.pos,
          size = v.size.map(_ ⇒ rect.dim),
          template = e.transformOption(v.template))
        vec = oldVec
        res
      case c: ConnectionDef if (vec != Vector2(0, 0)) ⇒
        c.copy(e.transformOption(c.a), e.transformOption(c.b), points = c.points map { _ + vec })
      case p: PortDef if (vec != Vector2(0, 0)) ⇒
        val vecp = Vector2(0, vec.y)
        p.copy(extPos = p.extPos + vecp, inPos = p.inPos + vecp)
    }: TreePF
  }
}