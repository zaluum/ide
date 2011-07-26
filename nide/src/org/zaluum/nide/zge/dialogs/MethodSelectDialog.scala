package org.zaluum.nide.zge.dialogs

import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.custom.CCombo
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog
import org.eclipse.swt.widgets.Combo
import net.miginfocom.swt.MigLayout
import org.eclipse.jface.dialogs.Dialog
import org.eclipse.jface.viewers._
import org.eclipse.swt.widgets.{ Shell, Composite, Text, Label, Control, Menu, MenuItem }
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{ SelectionListener, SelectionEvent }
import org.zaluum.nide.compiler._
import org.zaluum.nide.zge.{ Viewer, SWTScala }
import SWTScala._
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding

class MethodSelectDialog(viewer: Viewer, val vs: ValSymbol) extends Dialog(viewer.shell) {
  var comboC: CCombo = _
  var cviewer: ComboViewer = _
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    cviewer.getSelection().asInstanceOf[IStructuredSelection].getFirstElement match {
      case m: MethodBinding =>
        val tr = new EditTransformer() {
          val trans: PartialFunction[Tree, Tree] = {
            case v: ValDef if vs.decl == v ⇒
              v.copy(params = List(Param(InvokeExprType.signatureName, toMethodStr(m))))
          }
        }
        viewer.controller.exec(tr)

      case _ =>
    }
  }
  def toMethodStr(m:MethodBinding) = m.selector.mkString + m.signature().mkString
  override def createDialogArea(parent: Composite): Control = {
    val sup = super.createDialogArea(parent).asInstanceOf[Composite];
    val c = new Composite(sup, SWT.NONE)
    c.setLayout(new MigLayout)
    newLabel(c, "Method")
    val items: Array[MethodBinding] = vs.findPortInstance(InvokeExprType.obj) match {
      case Some(pi) =>
        pi.finalTpe match {
          case c: ClassJavaType =>
            val engine = ZaluumCompletionEngineScala.engineForVs(vs)
            val methods = ZaluumCompletionEngineScala.allMethods(engine, vs.owner.javaScope, c)
            methods.sortBy(_.selector.mkString).toArray

          case _ => Array()
        }
      case None => Array()
    }
    comboC = new CCombo(c, (SWT.DROP_DOWN | SWT.FLAT | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL))
    cviewer = new ComboViewer(comboC);
    cviewer.setContentProvider(new ArrayContentProvider)
    cviewer.setLabelProvider(new LabelProvider() {
      override def getText(element: AnyRef): String = {
        element match {
          case m: MethodBinding => m.toString()
        }
      }
    })
    cviewer.setInput(items)
    vs.params.values.headOption foreach { mstr =>
      items.find { m => toMethodStr(m) == mstr } foreach { m=>
    	  cviewer.setSelection(new StructuredSelection(m) )
      }
    }
    sup
  }
}