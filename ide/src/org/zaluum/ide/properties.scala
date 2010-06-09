package org.zaluum.ide

import org.eclipse.ui.views.properties.IPropertySource
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.zaluum.runtime._;
abstract class Property[T]{
  val desc:String 
  val get : ()=>T 
  val set: T=>_
}
case class StringProperty(desc:String, get : ()=>String, set : String=>_) extends Property[String]
case class BooleanProperty(desc:String, get : ()=>Boolean, set : Boolean=>_) extends Property[Boolean]

trait RefPropertySource[T<:Subject] extends BasePart[T] with IPropertySource{
  def getEditableValue = model  
  val properties : List[Property[_]] 
  lazy val getPropertyDescriptors : Array[IPropertyDescriptor] = (properties map { 
    _ match {
      case str: StringProperty => new TextPropertyDescriptor(str, str.desc)
      case b : BooleanProperty => new CheckboxPropertyDescriptor(b, b.desc)
    }
    }).toArray
  
  def isPropertySet(id : Object) = false 
  def resetPropertyValue(id : Object) { }
  override def getPropertyValue(id : Object) : Object =  id.asInstanceOf[Property[AnyRef]].get() 
  override def setPropertyValue(id:Object, value:Object)  {
    id.asInstanceOf[Property[AnyRef]].set(value)
    model.notifyObservers
  }
  abstract override def getAdapter(key: Class[_]) = {
    if (key == classOf[IPropertySource]) 
      this
    else super.getAdapter(key)
  }
}
  
class CheckboxPropertyDescriptor(id:Object,name:String) extends PropertyDescriptor(id,name) {
  import org.eclipse.jface.viewers.CellEditor;
  import org.eclipse.jface.viewers.CheckboxCellEditor;
  import org.eclipse.swt.widgets.Composite;
  override def createPropertyEditor(parent : Composite) : CellEditor = {
    val editor = new CheckboxCellEditor(parent);
    if (getValidator() != null)
      editor.setValidator(getValidator());
    editor;
  }
}
  
 