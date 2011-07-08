package org.zaluum.nide.zge

import org.zaluum.nide.compiler._

object Delete {
  def deleteSelection(selected: Set[Item], g:(BoxDef => Option[ConnectionGraph])) = {
    val selection = (for (i<-selected; s<-i.selectionSubject) yield s).toList
    val valDefs = selection collect { case v: ValDef ⇒ v }
    val portDefs = selection collect { case p: PortDef ⇒ p }
    val connDefs = selection.collect { case c: ConnectionDef ⇒ c } ++ 
      selection.collect { case LineSelectionSubject(c,l) => c}
    new EditTransformer() {
      def isRemoved (c : ConnectionDef) : Boolean = {
          def connectsRemovedVal(t:Tree) = t match {
            case p:PortRef => p.fromRef.symbol match {
              case v:ValSymbol => valDefs.contains(v.decl.asInstanceOf[ValDef])
              case _=> false
            }
            case _ => false
          }
          def connectsRemovedPortDef(t:Tree) = t match {
            case p:PortRef => p.symbol match { 
              case p:PortSymbol => selection.contains(p.decl)
              case _ => false
            }
            case _ => false
          }
          connectsRemovedPortDef(c.a) || connectsRemovedPortDef(c.b) ||
          connectsRemovedVal(c.a) || connectsRemovedVal(c.b) || connDefs.contains(c)
      }
      val removedDefs = valDefs map {_.symbol.asInstanceOf[ValSymbol].tpe} collect { 
          case b:BoxType if (b.decl!=null)=> b.decl
      }
      val trans: PartialFunction[Tree, Tree] = {
        case b: BoxDef ⇒
          val gb = g(b).get
          val removedEdges = for (e <- gb.edges; c <- e.srcCon; if isRemoved(c)) yield e
          val removedg =  removedEdges.foldLeft (gb)((gg,e) => gg.remove(e))
          val (newCons, newJunc) = removedg.prune.clean.toTree
          b.copy(
            defs=transformTrees(b.defs filterNot { removedDefs contains(_) }),
            vals=transformTrees(b.vals filterNot { valDefs contains (_) }) ,
            ports=transformTrees(b.ports filterNot { portDefs contains(_)} ),
            connections=newCons,
            junctions=newJunc) 
      }
    }
  }
}