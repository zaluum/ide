package org.zaluum.nide.zge
import org.eclipse.ui.views.properties.IPropertyDescriptor
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.BeanParamSymbol
import org.zaluum.nide.compiler.Values
import org.zaluum.nide.compiler.Param
import org.eclipse.ui.views.properties.IPropertySource2
import org.eclipse.swt.widgets.Display
import org.zaluum.nide.utils.SWTScala
import org.eclipse.ui.views.properties.PropertyDescriptor
import org.zaluum.nide.compiler.Name
import org.eclipse.ui.views.properties.TextPropertyDescriptor
import org.zaluum.nide.compiler.ParamSymbol
import org.zaluum.nide.compiler.ClassJavaType
import org.zaluum.nide.compiler.JavaType
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.DialogCellEditor
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.FontDialog
import org.eclipse.swt.widgets.Control
import org.zaluum.nide.zge.dialogs.OpenSearch
import org.eclipse.jdt.core.IJavaProject
import org.zaluum.nide.zge.dialogs.TextDialogCellEditor
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.zge.dialogs.MethodSelectDialog
import org.eclipse.jdt.internal.core.JavaProject
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.zaluum.nide.zge.dialogs.FieldSelectDialog

trait Property {
  def descriptor: IPropertyDescriptor
  def set(value: AnyRef)
  def get: AnyRef
  def isSet: Boolean
  def canBeReset = true
  def reset()
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
  def descriptor: IPropertyDescriptor = new PropertyDescriptor(this, "*Constructor")
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
trait ParamProperty extends Property {
  def key: Name
}
class TextParamProperty(controller: Controller, p: ParamSymbol, v: ValDef)
    extends ParamProperty {
  def key = p.name
  def descriptor: PropertyDescriptor = new TextPropertyDescriptor(this, p.name.str)
  def set(value: AnyRef) = {
    controller.exec(
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
class MethodParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef,
    tpe: ⇒ JavaType,
    static: Boolean) extends TextParamProperty(c, p, v) {
  def scope = v.sym.mainBS.scope
  def currentVal = v.sym.params.get(p).map { _.encoded }
  override def descriptor = new TextDialogPropertyDescriptor(this, p.name.str) {
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
class FieldParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef,
    tpe: ⇒ JavaType,
    static: Boolean) extends TextParamProperty(c, p, v) {
  def currentVal = v.sym.params.get(p).map { _.encoded }
  override def descriptor = new TextDialogPropertyDescriptor(this, p.name.str) {
    def openDialog(cell: Control) = new FieldSelectDialog(
      cell.getShell, tpe, static, v.sym, currentVal).openRet()
  }
}
class TypeParamProperty(
    c: Controller,
    p: ParamSymbol,
    v: ValDef) extends TextParamProperty(c, p, v) {
  override def descriptor = new TextDialogPropertyDescriptor(this, p.name.str) {
    def openDialog(cell: Control) =
      OpenSearch.openSearch(c.zproject.jProject, cell.getShell)
  }
}

class BeanProperty(
    controller: Controller,
    v: ValDef,
    val b: BeanParamSymbol) extends ParamProperty {
  lazy val tpe = Values.typeFor(b)
  def key = b.name
  def descriptor: IPropertyDescriptor = tpe.editor(this, b.name.str)
  def set(value: AnyRef) {
    if (get == value) return
    val encoded = tpe.parseSWT(value)
    controller.exec(
      if (encoded == "")
        v.removeParam(b.name)
      else
        v.addOrReplaceParam(Param(b.name, encoded)))
  }
  def get: AnyRef = v.sym.params.get(b) match {
    case Some(v) ⇒ v.toSWT
    case None    ⇒ tpe.defaultSWT
  }
  def isSet: Boolean = v.sym.params.contains(b)
  def reset() { controller.exec(v.removeParam(b.name)) }
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
/*
class BoxDefPopup(val viewer: TreeViewer, boxDef: BoxDef) extends Popup(viewer.shell, "Box") {
  val bs = boxDef.sym
  var clazz: String = ""
  var methSig: String = ""
  var methbutton: OpenButtonSelect = null
  var tpe: TpeEdit = null
  def exec {
    val res = clazz + "#" + methSig
    viewer.controller.exec(new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case b: BoxDef ⇒
          val initMethod = if (methSig == "") Some(clazz) else Some(res)
          b.copy(initMethod = initMethod, template = transform(b.template))
      }
    })
  }

  def populate(content: Composite) {
    val (cl, meth) = boxDef.initMethod match {
      case Some(s) ⇒ MethodBindingUtils.staticMethodSplit(s).getOrElse((s, ""))
      case None    ⇒ ("", "")
    }
      def button(str: String, info: String, btn: String)(body: ⇒ Unit) =
        new OpenButtonSelect(content, str, info, btn, body);
    /*def findMethods(static: Boolean) = new MethodSelectDialog(viewer) {
        def action(m: MethodWithNames) { methSig = m.methodSignature; exec }
        def static: Boolean = true
        def binding: TypeBinding = scope.lookupType(Name(clazz)).map { _.binding.asInstanceOf[TypeBinding] }.getOrElse(null)
        def scope: ZaluumClassScope = bs.scope
        def currentMethodSig: Option[String] = if (meth != "") Some(meth) else None
        def findMethods(engine: ZaluumCompletionEngine, scope: ZaluumClassScope, r: ReferenceBinding) =
          ZaluumCompletionEngineScala.allMethods(engine, scope, r, true)
      }*/
    tpe = new TpeEdit(content,
      "Init method class",
      getShell,
      viewer,
      cl,
      { str ⇒ clazz = str })

    /*methbutton = button("Init method", meth, "Select...") {
      findMethods(true).open
    }*/
  }
}
 * */
trait PropertySource extends IPropertySource2 {
  def display: Display
  var properties = List[Property]()
  def isPropertyResettable(id: AnyRef) = id.asInstanceOf[Property].canBeReset
  def isPropertySet(id: AnyRef) = id.asInstanceOf[Property].isSet
  def getEditableValue() = this
  def getPropertyDescriptors() = properties.map { _.descriptor }.toArray
  def getPropertyValue(id: AnyRef): AnyRef = id.asInstanceOf[Property].get
  def resetPropertyValue(id: AnyRef) = id.asInstanceOf[Property].reset()
  def setPropertyValue(id: AnyRef, swtValue: AnyRef) = SWTScala.async(display) {
    id.asInstanceOf[Property].set(swtValue)
  }
}