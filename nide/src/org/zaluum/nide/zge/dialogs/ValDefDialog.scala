package org.zaluum.nide.zge.dialogs

import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.fieldassist.AutoCompleteField
import org.eclipse.jface.fieldassist.ComboContentAdapter
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.zge.SWTScala.newCombo
import org.zaluum.nide.zge.SWTScala.newLabel
import org.zaluum.nide.zge.Viewer

import net.miginfocom.swt.MigLayout

class ValDefDialog(viewer: Viewer, val vs: ValSymbol) extends Dialog(viewer.shell) {
  def v = vs.decl.asInstanceOf[ValDef]
  var t: Combo = _
  def text = t.getText.trim;
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    if (v.typeName.str != t.getText) {
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case v: ValDef if vs.decl == v ⇒
            v.copy(typeName = Name(text))
        }
      }
      viewer.controller.exec(tr)
    }
  }
  def initial = v.typeName.str
  def proj = viewer.controller.zproject
  def proposals = proj.index().map { _.name.str }.sorted.toArray
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    c.setLayout(new MigLayout)
    newLabel(c, "Type")
    t = newCombo(c, initial, proposals, layout = "width 300,span")
    val auto = new AutoCompleteField(t, new ComboContentAdapter(), proposals.toArray)
    sup
  }
}