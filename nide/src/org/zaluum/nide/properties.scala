package org.zaluum.nide

import org.eclipse.ui.views.properties.IPropertySource
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.eclipse.ui.views.properties.PropertyDescriptor;
abstract class Property[T]{
  val desc:String 
  val get : ()=>T 
  val set: Option[T=>_]
}
case class StringProperty(desc:String, get : ()=>String, set : Option[String=>_]) extends Property[String]
case class BooleanProperty(desc:String, get : ()=>Boolean, set : Option[Boolean=>_]) extends Property[Boolean]
  
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
