package org.zaluum.nide.zge

import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.ConnectionDef
import org.zaluum.nide.compiler.ConnectionEnd
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.PortDef
import org.zaluum.nide.compiler.PortRef
import org.zaluum.nide.compiler.PortSymbol
import org.zaluum.nide.compiler.Template
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.compiler.SymbolTree

object Delete {
  def deleteSelection(selected: Set[Item], g: (Block ⇒ Option[ConnectionGraph])) = {
    val selection = (for (i ← selected; s ← i.selectionSubject) yield s).toList
    val valDefs = selection collect { case v: ValDef ⇒ v }
    val portDefs = selection collect { case p: PortDef ⇒ p }
    val connDefs = selection.collect { case c: ConnectionDef ⇒ c } ++
      selection.collect { case LineSelectionSubject(c, l) ⇒ c }
    new EditTransformer() {
      def isRemoved(c: ConnectionDef): Boolean = {
          def connectsRemovedVal(o: Option[ConnectionEnd]) = o match {
            case Some(p: PortRef) ⇒
              p.fromRef match {
                case s: SymbolTree[_] ⇒
                  s.sym match {
                    case v: ValSymbol ⇒ valDefs.contains(v.decl.asInstanceOf[ValDef])
                    case _            ⇒ false
                  }
                case _ ⇒ false
              }
            case _ ⇒ false
          }
          def connectsRemovedPortDef(o: Option[ConnectionEnd]) = o match {
            case Some(p: PortRef) ⇒ p.sym.pi.portSymbol match {
              case Some(p) ⇒ selection.contains(p.decl)
              case _       ⇒ false
            }
            case _ ⇒ false
          }
        connectsRemovedPortDef(c.a) || connectsRemovedPortDef(c.b) ||
          connectsRemovedVal(c.a) || connectsRemovedVal(c.b) || connDefs.contains(c)
      }
      val trans: PartialFunction[Tree, Tree] = {
        case t: Template ⇒
          t.copy(blocks = transformTrees(t.blocks),
            ports = transformTrees(t.ports.filterNot { portDefs contains }))
        case b: Block ⇒
          g(b) match {
            case Some(gb) ⇒ // maybe it's not shown so has no graph
              val removedEdges = for (e ← gb.edges; c ← e.srcCon; if isRemoved(c)) yield e
              val removedg = removedEdges.foldLeft(gb)((gg, e) ⇒ gg.remove(e))
              val (newCons, newJunc) = removedg.prune.clean.toTree
              b.copy(
                valDefs = transformTrees(b.valDefs filterNot { valDefs contains (_) }),
                parameters = transformTrees(b.parameters),
                connections = newCons,
                junctions = newJunc)
            case None ⇒ b
          }
      }
    }
  }
}