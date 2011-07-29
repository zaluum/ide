package org.zaluum.nide.zge.dialogs

import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.custom.CCombo
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
import org.zaluum.nide.zge.{Viewer,SWTScala}
import SWTScala._

class BoxDefDialog(viewer: Viewer, val b: BoxDef) extends Dialog(viewer.shell) {
  var textC:Text = _
  def text = textC.getText;
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand(){
    if (b.pkg.str != text) {  
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case box: BoxDef if box == b ⇒
            box.copy(pkg = Name(text), 
                defs=transformTrees(b.defs),
                vals=transformTrees(b.vals),
                ports=transformTrees(b.ports),
                connections=transformTrees(b.connections),
                junctions=transformTrees(b.junctions))
        }
      }
      viewer.controller.exec(tr)
    }
  }
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    c.setLayout(new MigLayout)
    newLabel(c,"Package")
    textC = newText(c,b.pkg.str,layout="width 300,span")
    sup
  }
}