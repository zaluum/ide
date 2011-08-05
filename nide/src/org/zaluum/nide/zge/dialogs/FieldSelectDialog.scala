package org.zaluum.nide.zge.dialogs

import java.util.Comparator
import scala.collection.JavaConversions
import org.eclipse.core.runtime.Status
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jface.dialogs.DialogSettings
import org.eclipse.jface.dialogs.IDialogSettings
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.widgets.Composite
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.ClassJavaType
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.InvokeExprType
import org.zaluum.nide.compiler.Param
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValSymbol
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.eclipse.integration.model.MethodUtils
import org.zaluum.nide.zge.Viewer
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.zaluum.nide.compiler.FieldAccessExprType

class FieldSelectDialog(viewer: Viewer, val vs: ValSymbol) extends FilteredItemsSelectionDialog2(viewer.shell, false) {
  override def isResizable = true
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    getSelectedItems().getFirstElement() match {
      case m: FieldBinding ⇒
        val tr = new EditTransformer() {
          val trans: PartialFunction[Tree, Tree] = {
            case v: ValDef if vs.decl == v ⇒
              v.copy(
                  template = transformOption(v.template),
                  params = List(Param(FieldAccessExprType.signatureName, m.name.mkString)))
          }
        }
        viewer.controller.exec(tr)
    }
  }
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

  val id = "org.zaluum.nide.methodSelectDialog"
  val settings = new DialogSettings(id);
  val items: Array[FieldBinding] = vs.findPortInstance(FieldAccessExprType.thiz) match {
    case Some(pi) ⇒
      pi.finalTpe match {
        case c: ClassJavaType ⇒
          val engine = ZaluumCompletionEngineScala.engineForVs(vs)
          val fields = ZaluumCompletionEngineScala.allFields(engine, vs.owner.template.asInstanceOf[BoxTypeSymbol].javaScope, c) // FIXME
          fields.sortBy(_.name.mkString).toArray
        case _ ⇒ Array()
      }
    case None ⇒ Array()
  }
  val currentFieldName = vs.params.values.headOption
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
    case _                  ⇒ null
  }
}

