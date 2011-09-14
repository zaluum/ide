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
import org.zaluum.nide.utils.SWTScala._
import org.zaluum.nide.zge.Viewer
import net.miginfocom.swt.MigLayout
import org.zaluum.nide.compiler.NewArrayExprType
import org.eclipse.swt.widgets.Text
import org.zaluum.nide.compiler.Param

class DimensionsDialog(viewer: Viewer, val vs: ValSymbol) extends Dialog(viewer.shell) {
  def v = vs.decl.asInstanceOf[ValDef]
  var t: Text = _
  def text = t.getText.trim;
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    if (v.typeName.str != t.getText) {
      val par = Param(NewArrayExprType.arrayDimName, text)
      val tr = v.addOrReplaceParam(par)
      viewer.controller.exec(tr)
    }
  }
  def initial = NewArrayExprType.dimensions(v)
  def proj = viewer.controller.zproject
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    c.setLayout(new MigLayout)
    newLabel(c, "Array dimensions")
    t = newText(c, initial, layout = "width 50,span")
    sup
  }
}