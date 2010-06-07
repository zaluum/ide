package org.zaluum.ide

import org.eclipse.ui.views.properties.IPropertySource
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.zaluum.runtime._;

class BoxProperty(box : VBox) extends IPropertySource {
  val NAME = "name";
  val IMAGE = "image";
  val model = box;
  val desc = { Array[IPropertyDescriptor](
      new TextPropertyDescriptor(NAME, "Name"),
      new TextPropertyDescriptor(IMAGE, "Image"))
  }
  override def getEditableValue() = { this.model }
  override def getPropertyDescriptors() : Array[IPropertyDescriptor] = { desc }
  override def isPropertySet(id : Object) : Boolean = { false }
  override def resetPropertyValue(id : Object) = { }
  override def getPropertyValue(id : Object) : Object = {
    id match {
      case NAME => return model.name
      case IMAGE => return null
    }
  }
  override def setPropertyValue(id:Object, value:Object) = {
    id match {
      case NAME => model.name_=(value.asInstanceOf[String])
      case IMAGE => 
    }
  }
}

class PortProperty(port : VPort) extends IPropertySource {
  val INPUT = "input";
  val TYPE = "type";
  val NAME = "name";
  val DIRECT = "direct";
  val LABEL = "label";
  val model = port;
  val desc = { Array[IPropertyDescriptor](
      new CheckboxPropertyDescriptor(INPUT, "Is input?"),
      new CheckboxPropertyDescriptor(DIRECT, "Is direct input?"),
      new TextPropertyDescriptor(TYPE, "Type"),
      new TextPropertyDescriptor(LABEL, "Label"),
      new TextPropertyDescriptor(NAME, "Name"))
  }
  override def getEditableValue() = { this.model }
  override def getPropertyDescriptors() : Array[IPropertyDescriptor] = { desc }
  override def isPropertySet(id : Object) : Boolean = { false }
  override def resetPropertyValue(id : Object) = { }
  override def getPropertyValue(id : Object) : Object = {
    id match {
      case INPUT => return null
      case NAME => return model.name
      case TYPE => return model.ttype
      case DIRECT => return null
      case LABEL => return null
    }
  }
  override def setPropertyValue(id:Object, value:Object) = {
    id match {
      case INPUT => 
      case NAME => model.name_=(value.asInstanceOf[String])
      case TYPE => model.ttype_=(value.asInstanceOf[String])
      case DIRECT => 
      case LABEL => 
    }
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
  
 