package org.zaluum.nide.zge

import org.zaluum.nide.compiler._

class SuperMenu(viewer: Viewer, vs: ValSymbol) extends ValDefDialog(viewer, vs) {
  override def execCommand() {
    def bd = vs.tpe.decl.asInstanceOf[BoxDef]
    if (v.typeName.str != text) {
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if bd == b ⇒
            b.copy(superName = Some(Name(text)),
              defs = transformTrees(b.defs),
              vals = transformTrees(b.vals),
              ports = transformTrees(b.ports),
              connections = transformTrees(b.connections),
              junctions = transformTrees(b.junctions))
        }
      }
      viewer.controller.exec(tr)
    }
  }
  def bs = vs.tpe.asInstanceOf[BoxTypeSymbol]
  override def initial = bs.superName.get.str;
}