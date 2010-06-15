package org.zaluum.runtime

object Debug2Model extends VisualModel{
  type B = DBox
  type C = ComposedDBox
  type W = DWire
  type P = DPort
  class DBox(
      val name:String,
      val fqName:String, 
      val ports:Set[DPort], 
      var parent:ComposedDBox) extends VBox
  
  class DPort(
      val name:String,
      val fqName:String,
      val slot:Slot,
      val ttype : String, 
      val link:String,
      val in:Boolean
    ) extends VPort {
      var value :String= "hola"
      var vbox : DBox = _
    }
  class DWire(val from:DPort, val to:DPort, val bendpoints:List[Bendpoint]) extends VWire 
  class ComposedDBox(name:String,fqName:String,ports:Set[DPort], parent:ComposedDBox,
      val connections:Set[DWire], val boxes:Set[DBox]) 
    extends DBox(name,fqName,ports,parent) with ComposedVBox
}