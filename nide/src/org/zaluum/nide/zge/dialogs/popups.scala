package org.zaluum.nide.zge.dialogs

import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog
import org.eclipse.jface.fieldassist.AutoCompleteField
import org.eclipse.jface.fieldassist.TextContentAdapter
import org.eclipse.jface.window.Window
import org.eclipse.swt.widgets.Button
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.SWT
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.FieldExprType
import org.zaluum.nide.compiler.InvokeExprType
import org.zaluum.nide.compiler.InvokeStaticExprType
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.NewArrayExprType
import org.zaluum.nide.compiler.NewExprType
import org.zaluum.nide.compiler.PortDef
import org.zaluum.nide.compiler.StaticExprType
import org.zaluum.nide.compiler.StaticFieldExprType
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.compiler.primitives
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine
import org.zaluum.nide.zge.SWTScala.addReaction
import org.zaluum.nide.zge.SWTScala.addTextReaction
import org.zaluum.nide.zge.SWTScala.newButton
import org.zaluum.nide.zge.ItemViewer
import org.zaluum.nide.zge.LabelItem
import org.zaluum.nide.zge.Popup
import org.zaluum.nide.zge.TreeViewer
import org.zaluum.nide.zge.ValDefItem
import org.zaluum.nide.compiler.Param
import org.zaluum.nide.compiler.SignatureExprType
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.zaluum.nide.compiler.CastToExprType
import org.zaluum.nide.compiler.LabelDesc
import org.zaluum.nide.compiler.Vector2

class PortDeclPopup(
  val viewer: TreeViewer, portDef: PortDef) extends Popup(viewer.shell, "Port " + portDef.name.str) {
  var nme: NmeSelect = _
  var tpe: TpeEdit = _
  def populate(content: Composite) {
    nme = new NmeSelect(content, portDef.name.str)
    tpe = new TpeEdit(content, "Type", getShell, viewer, portDef.typeName.str, { str ⇒ work })
    initialFocus = tpe.tpe
  }
  def work() {
    val command = portDef.renamePort(nme.value, Some(Name(tpe.value)))
    viewer.controller.exec(command)
    close()
  }
}

class ValDefPopup(val viewer: ItemViewer, fig: ValDefItem, gui: Boolean) extends Popup(viewer.shell,
  "Box " + fig.valDef.name.str) {
  def valDef = fig.valDef
  def v = valDef.sym
  def populate(content: Composite) {
      def button(str: String, info: String, btn: String)(body: ⇒ Unit) =
        new OpenButtonSelect(content, str, info, btn, body);
      def text(str: String, initial: String)(body: String ⇒ Unit) =
        new TextSelect(content, str, initial, body)
      def findMethods(static: Boolean) = new MethodSelectDialog(viewer, v) {
        def findMethods(engine: ZaluumCompletionEngine, scope: ZaluumClassScope, r: ReferenceBinding) =
          ZaluumCompletionEngineScala.allMethods(engine, scope, r, static)
      }
      def tpeMenu = new TpeSelect(content, "Box Type", getShell, viewer, valDef.typeName.str, {
        str ⇒ viewer.controller.exec(valDef.changeType(str)); close()
      })
      def label = text("Label", (if (gui) valDef.labelGui else valDef.label).map(_.description).getOrElse("")) { str ⇒
        viewer.controller.exec(valDef.editLabelAndRename(str))
        close()
      }
      def cons = button("Constructor", valDef.constructorParams.mkString(", "), "Edit...") { new ConstructorDialog(viewer, v).open() }
      def params = button("Parameters",
        valDef.params.map { t ⇒
          val p = t.asInstanceOf[Param]
          p.key.str + " = " + p.value.toString
        }.mkString(", "),
        "Edit...") { new ParamsDialog(viewer, v).open() }
      def staticMenu = {
        val classSymbol = v.tpe.asInstanceOf[StaticExprType].typeSymbol
        val value = v.params.get(classSymbol).getOrElse("")
        val target = new TpeSelect(content, "Target class", getShell, viewer, value.toString, {
          str ⇒
            viewer.controller.exec(valDef.addOrReplaceParam(
              Param(classSymbol.name, str)))
            close()
        })
      }
      def castTypeSelectMenu = {
        val initial = v.params.get(CastToExprType.typeSymbol) getOrElse { "" }
        new TpeEdit(content, "Cast to", getShell, viewer, initial.toString, { str ⇒
          viewer.controller.exec(valDef.addOrReplaceParam(Param(CastToExprType.typeName, str)))
          close()
        })
      }
      def methodName = v.info match {
        case m: MethodBinding ⇒ m.toString()
        case _ ⇒ "<none>"
      }
      def fieldName = v.info match {
        case f: FieldBinding ⇒ f.toString()
        case _ ⇒ "<none>"
      }
      def dimensions = NewArrayExprType.dimensions(valDef)
      def staticMethodMenu = button("Static method", methodName, "Select...") { findMethods(true).open }
      def methodMenu = button("Method", methodName, "Select...") { findMethods(false).open }
      def fieldMenu = button("Field", fieldName, "Select...") { new FieldSelectDialog(viewer, v).open }
      def dimensionsMenu = button("Dimensions", dimensions, "Change...") { new DimensionsDialog(viewer, v).open }
      def constructorSelectMenu = button("Constructor", methodName, "Select...") {
        new MethodSelectDialog(viewer, v) {
          def findMethods(engine: ZaluumCompletionEngine, scope: ZaluumClassScope, r: ReferenceBinding) =
            ZaluumCompletionEngineScala.allConstructors(engine, scope, r)
        }.open
      }
    fig match {
      case l: LabelItem ⇒ // 
      case _ ⇒
        tpeMenu
        label
        valDef.tpe match {
          case b: BoxTypeSymbol ⇒
            cons
            params
          case InvokeStaticExprType ⇒
            staticMenu
            staticMethodMenu
          case StaticFieldExprType ⇒
            staticMenu
            fieldMenu
          case InvokeExprType ⇒
            methodMenu
          case FieldExprType ⇒
            fieldMenu
          case NewExprType ⇒
            staticMenu
            constructorSelectMenu
          case NewArrayExprType ⇒
            staticMenu
            dimensionsMenu
          case CastToExprType ⇒
            castTypeSelectMenu
          case _ ⇒
            params
        }

    }
  }
}
class OpenButtonSelect(content: Composite, label: String, info: String, btnStr: String, body: ⇒ Unit) {
  val lblname = new Label(content, SWT.NONE)
  lblname.setText(label)
  lblname.setLayoutData("growx")
  val infolbl = new Label(content, SWT.NONE)
  infolbl.setText(info)
  infolbl.setLayoutData("growx")
  val btn = newButton(content, btnStr)
  btn.setLayoutData("growx, wrap")
  addReaction(btn) { body }
}
class TextSelect(content: Composite, label: String, initial: String, work: String ⇒ Unit) {
  val lblname = new Label(content, SWT.NONE)
  lblname.setText(label)
  lblname.setLayoutData("growx")
  val txt = new Text(content, SWT.BORDER)
  txt.setText(initial)
  txt.setLayoutData("span 2, growx, wrap")
  addTextReaction(txt) { work(txt.getText) }
}
class TpeSelect(content: Composite, str: String, shell: Shell, viewer: ItemViewer, initial: String, work: String ⇒ Unit) {
  new OpenButtonSelect(content, str, initial, "Search...", {
    OpenSearch.openSearch(viewer, shell) foreach { work }
  })
}
class NmeSelect(content: Composite, initial: String) {
  val lblname = new Label(content, SWT.NONE)
  lblname.setText("Name")
  val nme = new Text(content, SWT.BORDER)
  nme.setText(initial)
  nme.setLayoutData("span 2,growx,wrap")
  def value = nme.getText
  def run(work: () ⇒ Unit) = addTextReaction(nme) { work() }
}
object OpenSearch {
  def openSearch(viewer: ItemViewer, shell: Shell) = {
    val scope = SearchEngine.createJavaSearchScope(Array[IJavaElement](viewer.zproject.jProject))
    val o = new OpenTypeSelectionDialog(shell, false, PlatformUI.getWorkbench().getProgressService(), scope, IJavaSearchConstants.TYPE)
    if (o.open() == Window.OK) {
      val result = if (o.getResult == null) None else o.getResult.headOption
      result.map { _.asInstanceOf[IType].getFullyQualifiedName() }
    } else None

  }
}
class TpeEdit(content: Composite, lbl: String, shell: Shell, viewer: ItemViewer, initial: String, work: String ⇒ Unit) {
  val lbltype = new Label(content, SWT.NONE)
  lbltype.setText(lbl)
  val tpe = new Text(content, SWT.BORDER)
  tpe.setLayoutData("width 125::, growx")
  tpe.setText(initial)
  tpe.selectAll()
  addTextReaction(tpe) { work(tpe.getText) }
  val proposals = primitives.allTypes map { _.name.str } sorted;
  new AutoCompleteField(tpe, new TextContentAdapter(), proposals.toArray)
  val srchbtn = new Button(content, SWT.PUSH)
  srchbtn.setText("Search...")
  srchbtn.setLayoutData("align right, wrap")
  addReaction(srchbtn) {
    OpenSearch.openSearch(viewer, shell) foreach { tpe.setText }
  }
  def value = tpe.getText()
}
