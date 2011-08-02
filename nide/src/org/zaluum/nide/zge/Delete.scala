package org.zaluum.nide.zge

import org.zaluum.nide.compiler._

object Delete {
  def deleteSelection(selected: Set[Item], g:(Block => Option[ConnectionGraph])) = {
    val selection = (for (i<-selected; s<-i.selectionSubject) yield s).toList
    val valDefs = selection collect { case v: ValDef ⇒ v }
    val portDefs = selection collect { case p: PortDef ⇒ p }
    val connDefs = selection.collect { case c: ConnectionDef ⇒ c } ++ 
      selection.collect { case LineSelectionSubject(c,l) => c}
    new EditTransformer() {
      def isRemoved (c : ConnectionDef) : Boolean = {
          def connectsRemovedVal(o:Option[ConnectionEnd]) = o match {
            case Some(p:PortRef) => p.fromRef.symbol match {
              case v:ValSymbol => valDefs.contains(v.decl.asInstanceOf[ValDef])
              case _=> false
            }
            case _ => false
          }
          def connectsRemovedPortDef(o:Option[ConnectionEnd]) = o match {
            case Some(p:PortRef) => p.symbol match { 
              case p:PortSymbol => selection.contains(p.decl)
              case _ => false
            }
            case _ => false
          }
          connectsRemovedPortDef(c.a) || connectsRemovedPortDef(c.b) ||
          connectsRemovedVal(c.a) || connectsRemovedVal(c.b) || connDefs.contains(c)
      }
      val trans: PartialFunction[Tree, Tree] = {
        case b: Block ⇒
          val gb = g(b).get
          val removedEdges = for (e <- gb.edges; c <- e.srcCon; if isRemoved(c)) yield e
          val removedg =  removedEdges.foldLeft (gb)((gg,e) => gg.remove(e))
          val (newCons, newJunc) = removedg.prune.clean.toTree
          b.copy(
            valDefs=transformTrees(b.valDefs filterNot { valDefs contains (_) }) ,
            //ports=transformTrees(b.ports filterNot { portDefs contains(_)} ),
            parameters=transformTrees(b.parameters),
            connections=newCons,
            junctions=newJunc) 
      }
    }
  }
}