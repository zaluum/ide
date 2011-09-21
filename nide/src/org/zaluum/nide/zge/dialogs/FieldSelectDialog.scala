package org.zaluum.nide.zge.dialogs

import java.lang.Object
import java.util.Comparator

import scala.collection.JavaConversions

import org.eclipse.core.runtime.Status
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jface.dialogs.DialogSettings
import org.eclipse.jface.dialogs.IDialogSettings
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Shell
import org.zaluum.nide.compiler.JavaType
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala

class FieldSelectDialog(
    shell: Shell,
    tpe: ⇒ Option[JavaType],
    static: Boolean,
    val vs: ValSymbol,
    currentFieldName: Option[String]) extends FilteredItemsSelectionDialog2(shell, false) {
  override def isResizable = true
  override protected def okPressed() {
    getSelectedItems().getFirstElement() match {
      case m: FieldBinding ⇒ result = Some(m.name.mkString)
      case _               ⇒
    }
    super.okPressed()
  }
  var result: Option[String] = None
  def openRet() = { open(); result }
  object FieldLabelProvider extends LabelProvider {
    override def getText(element: Object) = {
      element match {
        case s: FieldBinding ⇒ s.name.mkString + " : " + s.`type`.readableName().mkString
        case _               ⇒ null
      }
    }
  }
  object FieldDetailsLabelProvider extends LabelProvider {
    override def getText(element: Object) = {
      element match {
        case s: FieldBinding ⇒ s.name.mkString + " : " + s.`type`.readableName().mkString
        case _               ⇒ null
      }
    }
  }

  val id = "org.zaluum.nide.fieldSelectDialog"
  val settings = new DialogSettings(id);
  val scope = vs.mainBS.scope

  val items: Array[FieldBinding] = tpe.map { _.binding } match {
    case Some(r: ReferenceBinding) ⇒
      val engine = ZaluumCompletionEngineScala.engineForVs(vs)
      val fields = ZaluumCompletionEngineScala.allFields(engine, scope, r, static)
      fields.sortBy(_.name.mkString).toArray
    case _ ⇒ Array()
  }
  val currentField = currentFieldName flatMap { mstr ⇒
    items.find { _.name.mkString == mstr }
  }
  setTitle("Select field");
  setMessage("Choose field " +
    (if (currentField.isEmpty)
      " - current field: " + currentFieldName.getOrElse("<missing>")
    else ""))
  setInitialPattern("**");
  setListLabelProvider(FieldLabelProvider);
  setDetailsLabelProvider(FieldDetailsLabelProvider);
  setInitialElementSelections(JavaConversions.seqAsJavaList(currentField.toList))
  class FieldItemsFilter extends ItemsFilter {
    if (this.getPattern() == null || this.getPattern == "") patternMatcher.setPattern("**")

    def isConsistentItem(item: AnyRef) = item.isInstanceOf[FieldBinding]
    def matchItem(item: Object) = item match {
      case f: FieldBinding ⇒ matches(f.name.mkString)
      case _               ⇒ false
    }
  }

  lazy val valuesToFill: java.lang.Iterable[_] = JavaConversions.asJavaIterable(items.toIterable)

  protected def getDialogSettings(): IDialogSettings = settings
  protected def getItemsComparator() = new Comparator[FieldBinding]() {
    def compare(m1: FieldBinding, m2: FieldBinding): Int = m1.name.mkString.compareTo(m2.name.mkString)
  }
  protected def validateItem(item: Object) = Status.OK_STATUS
  protected def createExtendedContentArea(parent: Composite) = null
  protected def createFilter = new FieldItemsFilter()
  def getElementName(item: Object) = item match {
    case f: FieldBinding ⇒ f.name.mkString
    case _               ⇒ null
  }
}

