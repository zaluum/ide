package org.zaluum.nide.zge.dialogs

import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.widgets.Menu
import org.eclipse.swt.widgets.MenuItem
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.InvokeExprType
import org.zaluum.nide.compiler.LabelDesc
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Vector2
import org.zaluum.nide.zge.LabelItem
import org.zaluum.nide.zge.ValDefItem
import org.zaluum.nide.zge.Viewer
import org.zaluum.nide.compiler.GetFieldExprType
import org.zaluum.nide.compiler.GetStaticFieldExprType
import org.zaluum.nide.compiler.InvokeStaticExprType
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.compiler.NewExprType

object ValDefMenu {
  def show(viewer: Viewer, fig: ValDefItem, gui: Boolean = false) {
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
      def staticMenu = newItem("Target class...") { new StaticSelectDialog(viewer, v).open }
      def findMethods(static:Boolean) = 
        new MethodSelectDialog(viewer, v) {
          def findMethods(engine: ZaluumCompletionEngine, scope: ZaluumClassScope, r: ReferenceBinding) = 
            ZaluumCompletionEngineScala.allMethods(engine, scope, r, static)
        }
      
      def methodMenu = newItem("Method...") { findMethods(false).open }
      def staticMethodMenu = newItem("Method...") { findMethods(true).open }
      def constructorSelectMenu = newItem("Constructor...") { 
        new MethodSelectDialog(viewer, v) {
          def findMethods(engine: ZaluumCompletionEngine, scope: ZaluumClassScope, r: ReferenceBinding) = 
            ZaluumCompletionEngineScala.allConstructors(engine, scope, r)
        }.open 
      }

      def fieldMenu = newItem("Field...") { new FieldSelectDialog(viewer, v).open }
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
              } else {
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
          case b: BoxTypeSymbol ⇒
            cons
            params
            tpeMenu
          case InvokeStaticExprType ⇒
            staticMenu
            staticMethodMenu
            tpeMenu
          case GetStaticFieldExprType ⇒
            staticMenu
            fieldMenu
            tpeMenu
          case InvokeExprType ⇒
            methodMenu
            tpeMenu
          case GetFieldExprType ⇒
            fieldMenu
            tpeMenu
          case NewExprType =>
            staticMenu
            constructorSelectMenu
          case _ ⇒
            params
            tpeMenu
        }
    }

    label.setSelection(if (gui) valDef.labelGui.isDefined else valDef.label.isDefined)
    menu.setVisible(true)
  }
}