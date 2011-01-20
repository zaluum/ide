package org.zaluum.nide.zge
import SWTScala._
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{ Composite, Button, Label, Text }
import org.zaluum.nide.model.Command

class PortDeclPopup(
  viewer: Viewer,
  pf: PortDeclFigure) extends ScrollPopup(viewer.shell) {
  def name = "Properties"
  def columns = 2
  def populate(content: Composite) {
    val lbl = new Label(content, SWT.NONE)
    lbl.setText("Type descriptor")
    val txt = new Text(content, SWT.BORDER)
    txt.setText(pf.portDecl.descriptor)
    val data = new GridData
    data.grabExcessHorizontalSpace = true;
    data.minimumWidth = 100;
    txt.setLayoutData(data)
    val ok = new Button(content, SWT.PUSH)
    ok.setText("OK")
    def work {
      val port = pf.portDecl
      val before = port.descriptor
      val now = txt.getText
      viewer.executeOrNotify(new Command {
        def redo = port.descriptor = now
        def undo = port.descriptor = before
        def canExecute = now != ""
      })
      hide
    }
    addReaction(txt) { work }
    addReaction(ok) { work }
  }
}