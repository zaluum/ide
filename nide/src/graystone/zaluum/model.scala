package graystone.zaluum

import scala.collection.mutable.Buffer

case class PortM(val name: String, val box: BoxM, val typ:String, val default:Option[String])
class BoxM (var name:String, var className:String){
  val inputs = Buffer[PortM]()
  val outputs = Buffer[PortM]()
  private def declare(name:String, buf:Buffer[PortM], typ : String, default:Option[String]) = {
    val port = new PortM(name,this,typ,default)
    buf += port
    port    
  }
  def  in (name : String, typ:String, default:String=null) =  declare(name,inputs,typ,Option(default))
  def  out (name : String, typ:String, default:String=null) = declare(name,outputs,typ,Option(default))
  var parameters = Map[String, String]()
}
class ConnectionBoxM(name:String,className:String) extends BoxM(name,className) {
  val boxes = Buffer[BoxM]()
  var connections = Buffer[(PortM, PortM)]()
}


