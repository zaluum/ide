package org.zaluum.nide.zge

import org.zaluum.nide.newcompiler.Tree
import org.zaluum.nide.newcompiler.EmptyTree
import SWTScala._
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{ Composite, Button, Label, Text }

class PortDeclPopup(
  viewer: TreeViewer,
  pf: PortDeclFigure) extends ScrollPopup(viewer.shell) {
  def name = "Properties"
  def columns = 2
  def populate(content: Composite) {
    val lbl = new Label(content, SWT.NONE)
    lbl.setText("Type descriptor")
    val txt = new Text(content, SWT.BORDER)
    txt.setText(pf.tree.typeName.str)
    val data = new GridData
    data.grabExcessHorizontalSpace = true;
    data.minimumWidth = 100;
    txt.setLayoutData(data)
    val ok = new Button(content, SWT.PUSH)
    ok.setText("OK")
    txt.setFocus
    def work {
      val port = pf.tree
      val now = txt.getText
      viewer.executeOrNotify(new TreeCommand {
        def canExecute = now != ""
        def execute(tree:Tree)= EmptyTree // TODO
      })
      hide
    }
    addReaction(txt) { work }
    addReaction(ok) { work }
  }
}