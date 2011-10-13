package org.zaluum.nide.zge

import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.jface.viewers.CheckboxCellEditor
import org.eclipse.jface.viewers.DialogCellEditor
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.SWT
import org.eclipse.ui.views.properties.PropertyDescriptor
import org.zaluum.nide.eclipse.TextDialogCellEditor

abstract class DialogPropertyDescriptor(id: AnyRef, displayName: String)
    extends PropertyDescriptor(id, displayName) {
  lazy val labelProvider = new LabelProvider() {
    override def getText(element: AnyRef) = element.toString()
  }
  setLabelProvider(labelProvider)
  def openDialog(cell: Control, value: AnyRef): String
  override protected def createPropertyEditor(parent: Composite): CellEditor = {
    new DialogCellEditor(parent) {
      override protected def openDialogBox(cell: Control) = openDialog(cell, doGetValue)
      override protected def updateContents(value: AnyRef) {
        if (getDefaultLabel != null) {
          getDefaultLabel.setText(labelProvider.getText(value))
        }
      }
    }
  }
}
class MultilineTextDescriptor(id: AnyRef, displayName: String) extends DialogPropertyDescriptor(id, displayName) {
  override lazy val labelProvider = new LabelProvider() {
    override def getText(element: AnyRef) =
      if (element != null)
        element.toString().lines.toStream.headOption.map(_.take(30) + "...").getOrElse("")
      else ""
  }
  def openDialog(cell: Control, value: AnyRef): String = {
    var res: String = null
    val d = new Dialog(cell.getShell) {
      var t: Text = null
      override def isResizable = true
      override def createDialogArea(parent: Composite) = {
        val c = super.createDialogArea(parent).asInstanceOf[Composite]
        t = new Text(c, SWT.MULTI | SWT.V_SCROLL | SWT.H_SCROLL)
        t.setText(value.toString)
        val d = new GridData(SWT.FILL, SWT.FILL, true, true)
        d.widthHint = 250
        d.heightHint = 250
        t.setLayoutData(d)
        c
      }
      override def okPressed() {
        res = t.getText();
        super.okPressed()
      }
    }
    d.open()
    res
  }
}

abstract class TextDialogPropertyDescriptor(id: AnyRef, displayName: String) extends DialogPropertyDescriptor(id, displayName) {
  override protected def createPropertyEditor(parent: Composite) = {
    new TextDialogCellEditor(parent) {
      override protected def openDialogBox(cell: Control) = openDialog(cell, doGetValue)
    }
  }
}
class CheckboxPropertyDescriptor(id: Object, name: String) extends PropertyDescriptor(id, name) {
  import org.eclipse.jface.viewers.CheckboxCellEditor;
  override def createPropertyEditor(parent: Composite) = {
    val editor = new CheckboxCellEditor(parent);
    if (getValidator() != null)
      editor.setValidator(getValidator());
    editor;
  }
}
