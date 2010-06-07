package org.zaluum.ide

import org.eclipse.ui.views.properties.IPropertySource
import org.eclipse.ui.views.properties.IPropertyDescriptor;
import org.eclipse.ui.views.properties.TextPropertyDescriptor;
import org.eclipse.ui.views.properties.PropertyDescriptor;
import org.zaluum.runtime._;

class BoxProperty(box : VBox) extends IPropertySource {
  val NAME : String = "name";
  val IMAGE : String = "image";
  val model : VBox = box;
  val desc : Array[IPropertyDescriptor] = { Array(
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
  val INPUT : String = "input";
  val TYPE : String = "type";
  val NAME : String = "name";
  val DIRECT : String = "direct";
  val LABEL : String = "label";
  val model : VPort = port;
  val desc : Array[IPropertyDescriptor] = { Array(
      new TextPropertyDescriptor(INPUT, "Is input?"),
      new TextPropertyDescriptor(DIRECT, "Is direct input?"),
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
  
//class CheckboxPropertyDescriptor(id:Object,name:String) extends PropertyDescriptor {
//  import org.eclipse.jface.viewers.CellEditor;
//  import org.eclipse.jface.viewers.CheckboxCellEditor;
//  import org.eclipse.swt.widgets.Composite;
//  override def createPropertyEditor(parent : Composite) : CellEditor = {
//    val editor : CellEditor = new CheckboxCellEditor(parent);
//    if (getValidator() != null)
//      editor.setValidator(getValidator());
//    editor;
//  }
//}
  
 