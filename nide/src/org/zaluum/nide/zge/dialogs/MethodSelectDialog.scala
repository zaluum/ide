package org.zaluum.nide.zge.dialogs

import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.custom.CCombo
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
import org.eclipse.jdt.internal.compiler.lookup.ExtraCompilerModifiers
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.zaluum.nide.eclipse.integration.model.MethodUtils
import org.eclipse.swt.widgets.Table
import org.eclipse.swt.widgets.TableColumn
import org.eclipse.swt.graphics.Image
import org.eclipse.jface.dialogs.IDialogSettings
import org.eclipse.jface.dialogs.DialogSettings
import java.util.Comparator
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.IProgressMonitor
import scala.collection.JavaConversions

class MethodSelectDialog(viewer: Viewer, val vs: ValSymbol) extends FilteredItemsSelectionDialog2(viewer.shell, false) {
  override def isResizable = true
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    getSelectedItems().getFirstElement() match {
      case m: MethodWithNames =>
        val tr = new EditTransformer() {
          val trans: PartialFunction[Tree, Tree] = {
            case v: ValDef if vs.decl == v ⇒
              v.copy(params = List(Param(InvokeExprType.signatureName, m.methodSignature)))
          }
        }
        viewer.controller.exec(tr)
    }
  }
  object MethodLabelProvider extends LabelProvider {
    override def getText(element: Object) = {
      element match {
        case s: MethodWithNames => s.text
        case _ => null
      }
    }
  }
  object MethodDetailsLabelProvider extends LabelProvider {
    override def getText(element: Object) = {
      element match {
        case s: MethodWithNames => s.fullText
        case _ => null
      }
    }
  }

  val id = "org.zaluum.nide.methodSelectDialog"
  val settings = new DialogSettings(id);
  val items: Array[MethodWithNames] = vs.findPortInstance(InvokeExprType.obj) match {
    case Some(pi) =>
      pi.finalTpe match {
        case c: ClassJavaType =>
          val engine = ZaluumCompletionEngineScala.engineForVs(vs)
          val methods = ZaluumCompletionEngineScala.allMethods(engine, vs.owner.javaScope, c)
          val jproject = viewer.zproject.jProject.asInstanceOf[JavaProject]
          val nameLookup = jproject.newNameLookup(Array[org.eclipse.jdt.core.ICompilationUnit]())
          val paramNames = methods map { m =>
            val names = MethodUtils.findMethodParamNames(m, jproject)
            val params = names.toList.flatMap(a => a)
            MethodWithNames(m, params)
          }
          paramNames.sortBy(_.selector).toArray
        case _ => Array()
      }
    case None => Array()
  }
  val currentMethodSig = vs.params.values.headOption
  val currentMethod = currentMethodSig flatMap { mstr =>
    items.find { _.methodSignature == mstr }
  }
  setTitle("Select method");
  setMessage("Choose method to invoke" +
    (if (currentMethod.isEmpty)
      " - current method signature: " + currentMethodSig.getOrElse("<missing>")
    else ""))
  setInitialPattern("**");
  setListLabelProvider(MethodLabelProvider);
  setDetailsLabelProvider(MethodDetailsLabelProvider);
  setInitialElementSelections(JavaConversions.seqAsJavaList(currentMethod.toList))
  class MethodItemsFilter extends ItemsFilter {
    if (this.getPattern() == null || this.getPattern == "") patternMatcher.setPattern("**")

    def isConsistentItem(item: AnyRef) = item.isInstanceOf[MethodWithNames]
    def matchItem(item: Object) = item match {
      case m: MethodWithNames => matches(m.text)
      case _ => false
    }
  }

  lazy val valuesToFill: java.lang.Iterable[_] = JavaConversions.asJavaIterable(items.toIterable)

  protected def getDialogSettings(): IDialogSettings = settings
  protected def getItemsComparator() = new Comparator[MethodWithNames]() {
    def compare(m1: MethodWithNames, m2: MethodWithNames): Int = m1.text.compareTo(m2.text)
  }
  protected def validateItem(item: Object) = Status.OK_STATUS
  protected def createExtendedContentArea(parent: Composite) = null
  protected def createFilter = new MethodItemsFilter()
  def getElementName(item: Object) = item match {
    case m: MethodWithNames => m.text
    case _ => null
  }
}

case class MethodWithNames(m: MethodBinding, paramNames: List[String]) {
  def flags = {
    val s = new StringBuffer()
    ASTNode.printModifiers(m.modifiers, s);
    s.toString
  }
  def returnStr = if (m.returnType != null) m.returnType.debugName() else "<no type>"
  def params = {
    if (m.parameters != null) {
      "(" +
        (if (m.parameters != Binding.NO_PARAMETERS) {
          val padded = paramNames.padTo(m.parameters.length, "?")
          val zip = padded.zip(m.parameters)
          zip.map {
            case (name, p) =>
              if (p != null) p.debugName() + " " + name else "<no argument type>"
          } mkString (", ")
        } else "") + ")"
    } else {
      "<no argument types>"
    }
  }
  def selector = m.selector.mkString
  def methodSignature = MethodUtils.toMethodSig(m)
  def fullText = MethodUtils.toMethodStr(m, paramNames) + " - " + m.declaringClass.debugName()
  def text = selector + " " + params + " : " + returnStr + " - " + m.declaringClass.debugName()
}