package org.zaluum.nide.zge.dialogs

import java.lang.Object
import java.lang.StringBuffer
import java.util.Comparator

import scala.collection.JavaConversions

import org.eclipse.core.runtime.Status
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jface.dialogs.DialogSettings
import org.eclipse.jface.dialogs.IDialogSettings
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Shell
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine

abstract class MethodSelectDialog(
    jproject: JavaProject,
    shell: Shell,
    binding: TypeBinding,
    scope: ZaluumClassScope,
    currentMethodSig: Option[String]) extends FilteredItemsSelectionDialog2(shell, false) {
  override def isResizable = true
  override protected def okPressed() {
    getSelectedItems().getFirstElement() match {
      case m: MethodWithNames ⇒ result = Some(m.methodSignature)
      case _                  ⇒
    }
    super.okPressed()
  }
  var result: Option[String] = None
  def openRet(): Option[String] = { open(); result }

  object MethodLabelProvider extends LabelProvider {
    override def getText(element: Object) = {
      element match {
        case s: MethodWithNames ⇒ s.text
        case _                  ⇒ null
      }
    }
  }
  object MethodDetailsLabelProvider extends LabelProvider {
    override def getText(element: Object) = {
      element match {
        case s: MethodWithNames ⇒ s.fullText
        case _                  ⇒ null
      }
    }
  }

  val id = "org.zaluum.nide.methodSelectDialog"
  val settings = new DialogSettings(id);
  def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding): List[MethodBinding]
  val items: Array[MethodWithNames] = binding match {
    case r: ReferenceBinding ⇒
      val engine = ZaluumCompletionEngineScala.engineFor(scope)
      val paramNames = findMethods(engine, r) map { m ⇒
        val names = org.zaluum.nide.utils.MethodBindingUtils.findMethodParamNames(m, jproject)
        val params = names.toList.flatMap(a ⇒ a)
        MethodWithNames(m, params)
      }
      paramNames.sortBy(_.selector).toArray
    case _ ⇒ Array()
  }
  val currentMethod = currentMethodSig flatMap { mstr ⇒
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
      case m: MethodWithNames ⇒ matches(m.text)
      case _                  ⇒ false
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
    case m: MethodWithNames ⇒ m.text
    case _                  ⇒ null
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
            case (name, p) ⇒
              if (p != null) p.debugName() + " " + name else "<no argument type>"
          } mkString (", ")
        } else "") + ")"
    } else {
      "<no argument types>"
    }
  }
  def selector = m.selector.mkString
  def declaringClass = new String(m.declaringClass.readableName())
  def methodSignature = org.zaluum.nide.utils.MethodBindingUtils.toMethodSig(m)
  def fullText = org.zaluum.nide.utils.MethodBindingUtils.toMethodStr(m, paramNames) + " - " + declaringClass
  def text = selector + " " + params + " : " + returnStr + " - " + declaringClass
}