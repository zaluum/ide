package org.zaluum.nide.zge.dialogs

import org.eclipse.jface.dialogs.Dialog
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.zge.SWTScala.newLabel
import org.zaluum.nide.zge.SWTScala.newText
import org.zaluum.nide.zge.Viewer

import net.miginfocom.swt.MigLayout

class BoxDefDialog(viewer: Viewer, val b: BoxDef) extends Dialog(viewer.shell) {
  var textC: Text = _
  def text = textC.getText;
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    if (b.pkg.str != text) {
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case box: BoxDef if box == b ⇒
            box.copy(pkg = Name(text),
              template = transform(b.template))
        }
      }
      viewer.controller.exec(tr)
    }
  }
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    c.setLayout(new MigLayout)
    newLabel(c, "Package")
    textC = newText(c, b.pkg.str, layout = "width 300,span")
    sup
  }
}