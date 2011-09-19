package org.zaluum.nide.zge
import java.lang.Object
import org.eclipse.jdt.core.search.IJavaSearchConstants
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.ui.dialogs.OpenTypeSelectionDialog
import org.eclipse.jface.viewers.CheckboxCellEditor
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.window.Window
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.dialogs.FilteredItemsSelectionDialog
import org.eclipse.ui.views.properties.IPropertyDescriptor
import org.eclipse.ui.views.properties.IPropertySource2
import org.eclipse.ui.views.properties.PropertyDescriptor
import org.eclipse.ui.views.properties.TextPropertyDescriptor
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.compiler.BeanParamSymbol
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.JavaType
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.NoSymbol
import org.zaluum.nide.compiler.Param
import org.zaluum.nide.compiler.ParamSymbol
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.Values
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine
import org.zaluum.nide.utils.MethodBindingUtils
import org.zaluum.nide.utils.SWTScala
import org.zaluum.nide.zge.dialogs.FieldSelectDialog
import org.zaluum.nide.zge.dialogs.MethodSelectDialog
import org.zaluum.nide.eclipse.TextDialogCellEditor
import org.eclipse.jface.viewers.DialogCellEditor
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.zaluum.nide.zge.dialogs.ConstructorSelectDialog
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.jface.viewers.ICellEditorValidator
import org.zaluum.nide.compiler.VarDecl

trait Property {
  def descriptor: IPropertyDescriptor
  def set(value: AnyRef)
  def get: AnyRef
  def isSet: Boolean
  def canBeReset = true
  def reset()
}
trait ControllerProperty extends Property {
  def c: Controller
  def displayName: String
}
trait ParamProperty extends ControllerProperty {
  def p: ParamSymbol
  def v: ValDef
  def key: Name = p.name
  def displayName = p.name.str
  def currentVal: Option[String] = v.sym.params.get(p).map { _.encoded }
  def set(value: AnyRef) = {
    c.exec(
      if (value == "") v.removeParam(key)
      else v.addOrReplaceParam(Param(key, value.toString)))
  }
  def get: AnyRef = v.params.asInstanceOf[List[Param]].find(_.key == key).map { _.value } getOrElse ("")
  def isSet = v.params.asInstanceOf[List[Param]].exists(_.key == key)
  def reset() { c.exec(v.removeParam(p.name)) }

}
trait NoResetProperty extends Property {
  override def canBeReset = false
  def reset() {}
}
abstract class InitProperty(b: BoxDef, val c: Controller) extends Property {
  def scope = b.sym.scope
  def className = split.map { _._1 }
  def methodName = split.map { _._2 }
  def split = b.initMethod.flatMap { s ⇒ MethodBindingUtils.staticMethodSplit(s) }
  def tpe = className.map { c ⇒ scope.getJavaType(Name(c)) }.getOrElse(NoSymbol)
  def set(cl: String, m: String) = {
    val comm = if (cl == "" && m == "") b.editInitMethod(None)
    else b.editInitMethod(Some(cl + "#" + m))
    c.exec(comm)
  }
  def isSet: Boolean = b.initMethod.isDefined
  def reset() { set("", "") }
}
class InitMethodProperty(b: BoxDef, c: Controller) extends InitProperty(b, c) with MethodProperty {
  val displayName = "*Init method"
  def currentVal = methodName
  def set(value: AnyRef) {
    set(className.getOrElse(""), value.toString)
  }
  def get: AnyRef = methodName.getOrElse("")
  def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding) =
    ZaluumCompletionEngineScala.allMethods(engine, scope, r, true)
}
class InitMethodClassProperty(b: BoxDef, c: Controller) extends InitProperty(b, c) with TypeProperty {
  val displayName = "*Init method class"
  def currentVal = className.orElse(b.initMethod)
  def set(value: AnyRef) {
    set(value.toString, methodName.getOrElse(""))
  }
  def get: AnyRef = className.orElse(b.initMethod).getOrElse("")
}
object ConstructorDeclProperty {
  lazy val Regexp = """\s*(\S+)\s*(\S+)\s*""".r
}
class ConstructorDeclProperty(boxDef: BoxDef, val c: Controller) extends Property {
  val descriptor = new TextPropertyDescriptor(this, "*Constructor parameters")
  descriptor.setValidator(new ICellEditorValidator() {
    def isValid(value: AnyRef) = {
      parse(value.toString) match {
        case Left(msg) ⇒ msg
        case _         ⇒ null
      }
    }
  })
  def parse(value: String): Either[String, List[VarDecl]] = {
    val params = value.toString.split(",").map(_.trim)
    Right(
      for (p ← params.toList) yield {
        p match {
          case ConstructorDeclProperty.Regexp(tpe, name) ⇒
            VarDecl(Name(name), Name(tpe))
          case _ ⇒
            return Left("Cannot parse parameter " + p + ". Format is \"Type Name, Type Name, ...\"");
        }
      })
  }
  def set(value: AnyRef) {
    parse(value.toString) match {
      case Left(_)  ⇒
      case Right(l) ⇒ c.exec(boxDef.editConstructor(l))
    }
  }
  // FIXME BUG updated twice because set value might not be equal to this (white space)
  // Use Value system?
  def get: AnyRef = boxDef.constructor.map { c ⇒ c.tpeName.str + " " + c.name.str }.mkString(", ")
  def isSet: Boolean = !boxDef.constructor.isEmpty
  def reset() { set("") }
}

class ValDefTypeProperty(valDef: ValDef, controller: Controller) extends NoResetProperty {
  def descriptor: IPropertyDescriptor = new PropertyDescriptor(this, "*Type")
  def set(value: AnyRef) { controller.exec(valDef.editType(Name(value.toString))) }
  def get: AnyRef = valDef.typeName.str
  def isSet: Boolean = true
}
class NameProperty(valDef: ValDef, controller: Controller) extends NoResetProperty {
  def descriptor = new PropertyDescriptor(this, "*Name")
  def set(value: AnyRef) {}
  def get = valDef.name.str
  def isSet = true
}
class LabelProperty(valDef: ValDef, controller: Controller, gui: Boolean) extends Property {
  def lbl = if (gui) valDef.labelGui else valDef.label
  def txt = if (gui) "*Label GUI" else "*Label"
  def descriptor: IPropertyDescriptor = new TextPropertyDescriptor(this, txt)
  def set(value: AnyRef) = controller.exec(valDef.editLabelAndRename(gui, value.toString))
  def get: AnyRef = lbl.map { _.description }.getOrElse("")
  def isSet: Boolean = lbl.isDefined
  def reset() = set("")
}

class ConstructorSelectProperty(valDef: ValDef, controller: Controller) extends Property {
  def descriptor = new DialogPropertyDescriptor(this, "*Constructor") {
    override lazy val labelProvider = new LabelProvider() {
      override def getText(element: AnyRef) = {
        element match {
          case (tpes: List[_], params: List[_]) ⇒ params.mkString(", ")
          case _                                ⇒ ""
        }
      }
    }
    def openDialog(cell: Control): Option[String] = {
      val c = new ConstructorSelectDialog(cell.getShell, valDef.sym)
      c.open()
      c.result foreach { comm ⇒
        SWTScala.async(cell.getDisplay) { controller.exec(comm) }
      }
      None
    }
  }
  def set(value: AnyRef) {} // done in dialog
  def get: AnyRef = (valDef.constructorTypes, valDef.constructorParams)
  def isSet: Boolean = valDef.constructorParams.isEmpty && valDef.constructorTypes.isEmpty
  def reset() = controller.exec(
    valDef.editConstructor(List(), List()))
}
class MissingParamProperty(controller: Controller, p: Param, v: ValDef) extends Property {
  def descriptor = new PropertyDescriptor(this, "<" + p.key.str + ">")
  def set(value: AnyRef) {}
  def get: AnyRef = p.value
  def isSet = true
  def reset = controller.exec(v.removeParam(p.key))
}

class TextParamProperty(val c: Controller, val p: ParamSymbol, val v: ValDef)
    extends ParamProperty {
  def descriptor: PropertyDescriptor = new TextPropertyDescriptor(this, p.name.str)
}
class ConstructorParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef,
    tpe: ⇒ JavaType) extends TextParamProperty(c, p, v) with MethodProperty {
  def scope = v.sym.mainBS.scope
  def tpe = tpe
  def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding) =
    ZaluumCompletionEngineScala.allConstructors(engine, scope, r)

}
trait MethodProperty extends ControllerProperty {
  def tpe: JavaType
  def scope: ZaluumClassScope
  def currentVal: Option[String]
  def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding): List[MethodBinding]
  override def descriptor = new TextDialogPropertyDescriptor(this, displayName) {
    def openDialog(cell: Control) = {
      val m = new MethodSelectDialog(
        c.zproject.jProject.asInstanceOf[JavaProject],
        cell.getShell(),
        tpe.binding,
        scope,
        currentVal) {
        def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding) =
          MethodProperty.this.findMethods(engine, r)
      }
      m.openRet()
    }
  }
}

class MethodParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef,
    tpe: ⇒ JavaType,
    val static: Boolean) extends TextParamProperty(c, p, v) with MethodProperty {
  def scope = v.sym.mainBS.scope
  def tpe = tpe
  def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding) =
    ZaluumCompletionEngineScala.allMethods(engine, scope, r, static)
}

class FieldParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef,
    tpe: ⇒ JavaType,
    static: Boolean) extends TextParamProperty(c, p, v) {
  override def descriptor = new TextDialogPropertyDescriptor(this, p.name.str) {
    def openDialog(cell: Control) = new FieldSelectDialog(
      cell.getShell, tpe, static, v.sym, currentVal).openRet()
  }
}

trait TypeProperty extends ControllerProperty {
  def currentVal: Option[String]
  override def descriptor = new TextDialogPropertyDescriptor(this, displayName) {
    def openDialog(cell: Control) =
      OpenSearch.openSearch(c.zproject.jProject, cell.getShell, currentVal)
  }
}
class TypeParamProperty(
  c: Controller,
  p: ParamSymbol,
  v: ValDef) extends TextParamProperty(c, p, v) with TypeProperty

object OpenSearch {
  def openSearch(project: IJavaProject, shell: Shell, initial: Option[String]) = {
    val scope = SearchEngine.createJavaSearchScope(Array[IJavaElement](project))
    val o = new OpenTypeSelectionDialog(shell, false, PlatformUI.getWorkbench().getProgressService(), scope, IJavaSearchConstants.TYPE)
    initial foreach { s ⇒
      o.setInitialPattern(s, FilteredItemsSelectionDialog.FULL_SELECTION)
    }
    if (o.open() == Window.OK) {
      val result = if (o.getResult == null) None else o.getResult.headOption
      result.map { _.asInstanceOf[IType].getFullyQualifiedName() }
    } else None

  }
}
class BeanProperty(
    val c: Controller,
    val v: ValDef,
    val p: BeanParamSymbol) extends ParamProperty {
  lazy val tpe = Values.typeFor(p)
  def descriptor: IPropertyDescriptor = tpe.editor(this, p.name.str)
  override def set(value: AnyRef) {
    if (get == value) return
    val encoded = tpe.parseSWT(value)
    super.set(encoded)
  }
  override def get: AnyRef = v.sym.params.get(p) match {
    case Some(v) ⇒ v.toSWT
    case None    ⇒ tpe.defaultSWT
  }
}
abstract class DialogPropertyDescriptor(id: AnyRef, displayName: String)
    extends PropertyDescriptor(id, displayName) {
  lazy val labelProvider = new LabelProvider() {
    override def getText(element: AnyRef) = element.toString()
  }
  setLabelProvider(labelProvider)
  def openDialog(cell: Control): Option[String]
  override protected def createPropertyEditor(parent: Composite): CellEditor = {
    new DialogCellEditor(parent) {
      override protected def openDialogBox(cell: Control) = openDialog(cell)
      override protected def updateContents(value: AnyRef) {
        if (getDefaultLabel != null) {
          getDefaultLabel.setText(labelProvider.getText(value))
        }
      }
    }
  }
}
abstract class TextDialogPropertyDescriptor(id: AnyRef, displayName: String) extends DialogPropertyDescriptor(id, displayName) {
  override protected def createPropertyEditor(parent: Composite) = {
    new TextDialogCellEditor(parent) {
      override protected def openDialogBox(cell: Control) = openDialog(cell)
    }
  }
}
class CheckboxPropertyDescriptor(id: Object, name: String) extends PropertyDescriptor(id, name) {
  import org.eclipse.jface.viewers.CheckboxCellEditor;
  override def createPropertyEditor(parent: Composite) = {
    val editor = new CheckboxCellEditor(parent);
    if (getValidator() != null)
      editor.setValidator(getValidator());
    editor;
  }
}
trait PropertySource extends IPropertySource2 {
  def display: Display
  var properties = List[Property]()
  def isPropertyResettable(id: AnyRef) = id.asInstanceOf[Property].canBeReset
  def isPropertySet(id: AnyRef) = id.asInstanceOf[Property].isSet
  def getEditableValue() = this
  def getPropertyDescriptors() = { properties.map { _.descriptor }.toArray }
  def getPropertyValue(id: AnyRef): AnyRef = id.asInstanceOf[Property].get
  def resetPropertyValue(id: AnyRef) = id.asInstanceOf[Property].reset()
  def setPropertyValue(id: AnyRef, swtValue: AnyRef) = SWTScala.async(display) {
    val prop = id.asInstanceOf[Property]
    if (prop.get != swtValue)
      prop.set(swtValue)
  }
}