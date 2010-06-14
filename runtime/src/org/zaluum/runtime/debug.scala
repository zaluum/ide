package org.zaluum.runtime

object DebugModel extends VisualModel{
  type B = DBox
  type C = ComposedDBox
  type W = DWire
  type P = DPort
  case class DBox(
    val name:String,
    val fqName:String,
    val ports:Set[DPort],
    val parent:ComposedDBox
  ) extends VBox  
  
  case class DPort(
    val name:String,
    val fqName:String,
    val slot:Slot,
    val ttype : String, 
    val link:String,
    val vbox : DBox,
    val in:Boolean
  )extends VPort {
    def value:String = "hola"
  }
  case class DWire(val from:DPort, val to:DPort, val bendpoints:List[Bendpoint]) extends VWire
  case class ComposedDBox(
      val name :String,
      val fqName:String,
      val ports:Set[DPort],
      val parent:ComposedDBox,
      val connections:Set[DWire],
      val boxes : Set[DBox]
      )
      extends ComposedVBox 
}