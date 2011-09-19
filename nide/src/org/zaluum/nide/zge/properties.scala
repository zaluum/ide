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
import org.zaluum.nide.zge.dialogs.TextDialogCellEditor

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
  val static = true
  def currentVal = methodName
  def set(value: AnyRef) {
    set(className.getOrElse(""), value.toString)
  }
  def get: AnyRef = methodName.getOrElse("")
}
class InitMethodClassProperty(b: BoxDef, c: Controller) extends InitProperty(b, c) with TypeProperty {
  val displayName = "*Init method class"
  def currentVal = className.orElse(b.initMethod)
  def set(value: AnyRef) {
    set(value.toString, methodName.getOrElse(""))
  }
  def get: AnyRef = className.orElse(b.initMethod).getOrElse("")
}
class ValDefTypeProperty(valDef: ValDef, controller: Controller) extends Property {
  def descriptor: IPropertyDescriptor = new PropertyDescriptor(this, "*Type")
  def set(value: AnyRef) { controller.exec(valDef.editType(Name(value.toString))) }
  def get: AnyRef = valDef.typeName.str
  def isSet: Boolean = true
  override def canBeReset = false
  def reset() {}
}
class NameProperty(valDef: ValDef, controller: Controller) extends Property {
  def descriptor = new PropertyDescriptor(this, "*Name")
  def set(value: AnyRef) {}
  def get = valDef.name.str
  override def canBeReset = false
  def isSet = true
  def reset() {}
}
class LabelProperty(valDef: ValDef, controller: Controller, gui: Boolean) extends Property {
  def lbl = if (gui) valDef.labelGui else valDef.label
  def txt = if (gui) "*Label GUI" else "*Label"
  def descriptor: IPropertyDescriptor = new TextPropertyDescriptor(this, txt)
  def set(value: AnyRef) {
    controller.exec(valDef.editLabelAndRename(gui, value.toString))
  }
  def get: AnyRef = lbl.map { _.description }.getOrElse("")
  def isSet: Boolean = lbl.isDefined
  def reset() = set("")
}
class ConstructorProperty(valDef: ValDef, controller: Controller) extends Property {
  def descriptor = new PropertyDescriptor(this, "*Constructor")
  def set(value: AnyRef) {}
  def get: AnyRef = (valDef.constructorTypes, valDef.constructorParams)
  def isSet: Boolean =
    valDef.constructorParams.isEmpty && valDef.constructorTypes.isEmpty
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
  def set(value: AnyRef) = {
    c.exec(
      if (value == "") v.removeParam(key)
      else v.addOrReplaceParam(Param(key, value.toString)))
  }
  def get = v.params.asInstanceOf[List[Param]].find(_.key == key).map { _.value } getOrElse ("")
  def isSet = v.params.asInstanceOf[List[Param]].exists(_.key == key)
  def reset = set("")
}
class ConstructorParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef,
    tpe: ParamSymbol) extends TextParamProperty(c, p, v) {
  override def descriptor = new PropertyDescriptor(this, p.name.str)
}
trait MethodProperty extends Property {
  def c: Controller
  def tpe: JavaType
  def scope: ZaluumClassScope
  def static: Boolean
  def currentVal: Option[String]
  def displayName: String
  override def descriptor = new TextDialogPropertyDescriptor(this, displayName) {
    def openDialog(cell: Control) = {
      val m = new MethodSelectDialog(
        c.zproject.jProject.asInstanceOf[JavaProject],
        cell.getShell(),
        tpe.binding,
        scope,
        currentVal) {
        def findMethods(engine: ZaluumCompletionEngine, r: ReferenceBinding) =
          ZaluumCompletionEngineScala.allMethods(engine, scope, r, static)
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
  def set(value: AnyRef) {
    if (get == value) return
    val encoded = tpe.parseSWT(value)
    c.exec(
      if (encoded == "")
        v.removeParam(p.name)
      else
        v.addOrReplaceParam(Param(p.name, encoded)))
  }
  def get: AnyRef = v.sym.params.get(p) match {
    case Some(v) ⇒ v.toSWT
    case None    ⇒ tpe.defaultSWT
  }
  def isSet: Boolean = v.sym.params.contains(p)
  def reset() { c.exec(v.removeParam(p.name)) }
}
abstract class TextDialogPropertyDescriptor(id: AnyRef, displayName: String) extends PropertyDescriptor(id, displayName) {
  setLabelProvider(new LabelProvider() {
    override def getText(element: AnyRef) = element.toString()
  })
  def openDialog(cell: Control): Option[String]
  override protected def createPropertyEditor(parent: Composite) = {
      def validator = getValidator
    new TextDialogCellEditor(parent) {
      setValidator(validator)
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
    id.asInstanceOf[Property].set(swtValue)
  }
}