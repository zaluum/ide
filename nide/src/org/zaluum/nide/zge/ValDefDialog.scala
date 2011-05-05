package org.zaluum.nide.zge

import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.custom.CCombo
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog
import org.eclipse.swt.widgets.Combo
import org.eclipse.jface.fieldassist.ComboContentAdapter
import org.eclipse.jface.fieldassist.AutoCompleteField
import net.miginfocom.swt.MigLayout
import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.viewers._
import org.eclipse.swt.widgets.{Shell, Composite, Text, Label, Control, Menu, MenuItem}
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{SelectionListener,SelectionEvent}
import org.zaluum.nide.compiler._
import SWTScala._

class ValDefDialog(viewer: Viewer, val vs: ValSymbol) extends Dialog(viewer.shell) {
  def v = vs.decl.asInstanceOf[ValDef]
  var t :CCombo = _
  def text = t.getText.trim;
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand(){
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
  def proj = viewer.controller.global
  def proposals = proj.index.map{_.str}.sorted.toArray
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    c.setLayout(new MigLayout)
    newLabel(c,"Type")
    t = newCombo(c,initial,proposals,layout="width 300,span")
    val auto = new AutoCompleteField(t,new ComboContentAdapter(),proposals.toArray)
    sup
  }
}