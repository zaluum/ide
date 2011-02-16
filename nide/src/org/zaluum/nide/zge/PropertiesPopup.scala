package org.zaluum.nide.zge

import org.zaluum.nide.newcompiler.EditTransformer
import org.zaluum.nide.newcompiler.PortDef
import org.zaluum.nide.newcompiler.Name

import SWTScala._
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.{ Composite, Button, Label, Text }
import org.zaluum.nide.newcompiler.{ EmptyTree, Tree }

class PortDeclPopup(
  viewer: TreeViewer,
  portDef : PortDef) extends ScrollPopup(viewer.shell) {
  def name = "Properties"
  def columns = 2
  def populate(content: Composite) {
    val lbl = new Label(content, SWT.NONE)
    lbl.setText("Type descriptor")
    val txt = new Text(content, SWT.BORDER)
    txt.setText(portDef.typeName.str)
    val data = new GridData
    data.grabExcessHorizontalSpace = true;
    data.minimumWidth = 100;
    txt.setLayoutData(data)
    val ok = new Button(content, SWT.PUSH)
    ok.setText("OK")
    txt.setFocus
    def work {
      val port = portDef
      val now = txt.getText
      if (now!=null) {
        val name = Name(now)
        viewer.controller.exec(new EditTransformer {
          val trans : PartialFunction[Tree,Tree] = {
            case p:PortDef if (p==port) => p.copy(typeName = name)
          }
        })
      }
      hide
    }
    addReaction(txt) { work }
    addReaction(ok) { work }
  }
}