package org.zaluum.nide.zge.dialogs

import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.zge.Viewer
class SuperDialog(viewer: Viewer, vs: ValSymbol) extends ValDefDialog(viewer, vs) {
  override def execCommand() {
      def bd = vs.tpe.decl.asInstanceOf[BoxDef]
    if (v.typeName.str != text) {
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if bd == b ⇒
            b.copy(superName = Some(Name(text)), template = transform(b.template))
        }
      }
      viewer.controller.exec(tr)
    }
  }
  def bs = vs.tpe.asInstanceOf[BoxTypeSymbol]
  override def initial = bs.superName.get.str;
}