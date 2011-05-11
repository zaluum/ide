package org.zaluum.nide.zge.dialogs

import org.zaluum.nide.zge.LabelItem
import org.zaluum.nide.zge.ValDefItem
import org.zaluum.nide.zge.Viewer
import net.miginfocom.swt.MigLayout
import org.eclipse.jface.dialogs.Dialog
import org.eclipse.swt.widgets.{ Shell, Composite, Label, Control, Menu, MenuItem }
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{ SelectionListener, SelectionEvent }
import org.zaluum.nide.compiler._

object ValDefMenu {
  def show(viewer: Viewer, fig: ValDefItem, gui:Boolean=false) {
    val valDef = fig.valDef
    val v = fig.valSym
    val menu = new Menu(viewer.shell, SWT.POP_UP)
    def newItem(str: String, check: Boolean = false)(action: ⇒ Unit) = {
      val item = new MenuItem(menu, if (check) SWT.CHECK else SWT.PUSH)
      item.setText(str)
      item.addSelectionListener(new SelectionListener() {
        def widgetSelected(e: SelectionEvent) {
          action
        }
        def widgetDefaultSelected(e: SelectionEvent) {}
      })
      item
    }
    def tpeMenu = newItem("Type...") { new ValDefDialog(viewer, v).open() }
    def params = newItem("Parameters...") { new ParamsDialog(viewer, v).open() }
    def cons = newItem("Constructor...") { new ConstructorDialog(viewer, v).open() }
    def label = newItem("Show label", true) {
      val tr = new EditTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case v: ValDef if valDef == v ⇒
            def newLabel = Some(LabelDesc(v.name.str, Vector2(0, 0)))
            if (gui) {
              val lbl = if (v.labelGui.isDefined) None else newLabel
              v.copy(labelGui = lbl)
            }else{
              val lbl = if (v.label.isDefined) None else newLabel
              v.copy(label = lbl)
            }
        }
      }
      viewer.controller.exec(tr)
    }
    fig match {
      case l: LabelItem ⇒ // 
      case _ ⇒
        valDef.tpe match {
          case b: BoxTypeSymbol if (!b.isLocal) ⇒
            cons
            params
            tpeMenu
          case b: BoxTypeSymbol if (b.isLocal) ⇒
            newItem("Super...") {
              new SuperDialog(viewer, v).open()
            }
            params
          case _ ⇒
            tpeMenu
        }
    }
    
    label.setSelection(if (gui) valDef.labelGui.isDefined else valDef.label.isDefined)
    menu.setVisible(true)
  }
}