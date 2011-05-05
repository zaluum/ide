package org.zaluum.nide.zge

import net.miginfocom.swt.MigLayout
import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.viewers._
import org.eclipse.swt.widgets.{Shell, Composite, Label, Control, Menu, MenuItem}
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{SelectionListener,SelectionEvent}
import org.zaluum.nide.compiler._

object ValDefMenu{
  def show(viewer:Viewer,v:ValSymbol) {
    val menu = new Menu(viewer.shell, SWT.POP_UP)
    def newItem(str:String)(action: => Unit) {
      val item = new MenuItem(menu,SWT.PUSH)
      item.setText(str)
      item.addSelectionListener(new SelectionListener() {
        def widgetSelected(e:SelectionEvent){
          action
        }
        def widgetDefaultSelected(e:SelectionEvent){}
      })
    }
    def tpeMenu = newItem("Type...") { new ValDefDialog(viewer, v).open() } 
    v.tpe match {
      case b:BoxTypeSymbol if (!b.isLocal)=>
        newItem("Constructor...") {
          new ConstructorDialog(viewer, v).open()
        }
        tpeMenu
      case b:BoxTypeSymbol if (b.isLocal) =>
        newItem("Super...") {
          new SuperDialog(viewer,v).open() 
        }
      case _ =>
        tpeMenu
    }
    menu.setVisible(true)
  }
}