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
import org.zaluum.nide.compiler.FieldExprType
import org.zaluum.nide.compiler.StaticExprType
import org.zaluum.nide.compiler.SignatureExprType
import org.zaluum.nide.compiler.StaticFieldExprType
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.compiler.ThisExprType

class FieldSelectDialog(viewer: Viewer, val vs: ValSymbol) extends FilteredItemsSelectionDialog2(viewer.shell, false) {
  override def isResizable = true
  override protected def okPressed() {
    execCommand()
    super.okPressed()
  }
  def execCommand() {
    getSelectedItems().getFirstElement() match {
      case m: FieldBinding ⇒
        val sig = vs.tpe.asInstanceOf[SignatureExprType].signatureName
        val tr = vs.tdecl.addOrReplaceParam(Param(sig, m.name.mkString))
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
  val static = vs.tpe.isInstanceOf[StaticExprType]
  val scope = vs.owner.template.asInstanceOf[BoxTypeSymbol].javaScope // FIXME 

  val binding = vs.tpe match {
    case t:ThisExprType ⇒ t.thisPort(vs).finalTpe.binding
    case s:StaticExprType  ⇒
      vs.classinfo match {
        case cl: ClassJavaType ⇒ cl.binding
        case _                 ⇒ null
      }
  }
  val items: Array[FieldBinding] = binding match {
    case r: ReferenceBinding ⇒
      val engine = ZaluumCompletionEngineScala.engineForVs(vs)
      val fields = ZaluumCompletionEngineScala.allFields(engine, scope, r, static)
      fields.sortBy(_.name.mkString).toArray
    case _ ⇒ Array()
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
    case _               ⇒ null
  }
}

