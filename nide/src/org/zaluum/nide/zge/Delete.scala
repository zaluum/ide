package org.zaluum.nide.zge

import org.zaluum.nide.compiler._

object Delete {
  def deleteSelection(selected: Set[Item]) = {
    val selectedTree = selected map { _.tree.asInstanceOf[Tree] }
    val valDefs = selectedTree collect { case v: ValDef ⇒ v }
    val portDefs = selectedTree collect { case p: PortDef ⇒ p }
    val connDefs = selectedTree collect { case c: ConnectionDef ⇒ c }
    // add valDefs connections
    new EditTransformer() {
      def filter(l: List[Tree]) : List[Tree] = l.filterNot(selectedTree.contains(_))
      def filterConnections (l:List[Tree]) : List[Tree] = {
        val connections = filter(l).asInstanceOf[List[ConnectionDef]]
        connections filterNot { c=> // true for removal 
          def connectsRemovedVal(t:Tree) = t.asInstanceOf[PortRef].fromRef.symbol match {
            case v:ValSymbol => selectedTree.contains(v.decl)
            case _=> false
          }
          def connectsRemovedPortDef(t:Tree) = t.asInstanceOf[PortRef].symbol match {
            case p:PortSymbol => selectedTree.contains(p.decl)
            case _ => println("no symbol for port tree " + t); false
          }
          connectsRemovedPortDef(c.a) || connectsRemovedPortDef(c.b) ||
          connectsRemovedVal(c.a) || connectsRemovedVal(c.b)
        }
      }
      def filterDefs(l:List[Tree]) = {
        val removedDefs = valDefs map {_.symbol.asInstanceOf[ValSymbol].tpe} collect { 
            case b:BoxTypeSymbol if (b.decl!=null)=> b.decl
        }
        l.filterNot { removedDefs.contains(_)}
      }
      val trans: PartialFunction[Tree, Tree] = {
        case b: BoxDef ⇒
          BoxDef(b.name, b.superName, b.image,
            transformTrees(filterDefs(b.defs)),
            transformTrees(filter(b.vals)),
            transformTrees(filter(b.ports)),
            transformTrees(filterConnections(b.connections)))
      }
    }
  }
}