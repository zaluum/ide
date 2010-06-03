package org.zaluum.runtime
import serial.ModelProtos._
import scala.collection.mutable.Set

class PBox extends VBox {
  var parent : ComposedVBox = _
  var ports = Set[VPort]()
  override var name = ""
  def fqName = ""
}
class PPort extends VPort {
  override var vbox : VBox = _
  override var ttype = ""
  override var slot = Slot(0,true)
  def fqName = ""
  override var name = ""
}
class ComposedPBox extends PBox with ComposedVBox {
  var connections = Set[VWire]()
  var boxes = Set[VBox]()
}
class PWire extends VWire {
  var bendpoints = List()
  var from :PPort= _
  var to : PPort= _ 
}

/*trait Command {
  def execute()
  def undo()
  def redo()
}*/
/*class CreateBoxCommand extends Command{
 def execute() 
}*/