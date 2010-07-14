package org.zaluum.example;
import org.zaluum.runtime.{Box, InPort,OutPort,ComposedBox}
import org.zaluum.drivers.robotis.{RobotisDriver, RobotisFeedback}
class Op(name:String,parent:ComposedBox)(op : Int => Int) extends Box(name,parent) {
  val in = InPort("in",0)
  val out = OutPort("out",0)
  override def act { out.v = op(in.v) }
}
class Const(name : String, parent:ComposedBox, const : Int) extends Box(name,parent) {
  val  out = OutPort("out",const)
}
class Forward(name:String,parent:ComposedBox) extends Op(name,parent)({x=>x})

trait Motor {
  self : Box => 
  val targetSpeed = InPort("targetSpeed",0.0)
}
trait Overdrive {
  self : Box => 
  val overdriven = OutPort("over",false)
  val resetOver = InPort("resetover",false)
}
trait RPMSensor {
  self : Box =>
  val speed = OutPort("speed",0.0)
}

class AX12Motor(name:String,parent:ComposedBox,driver : RobotisDriver, id:Int) extends ComposedBox(name,parent) with
    Motor with Overdrive with RPMSensor {
  val demux = new Box("demux",this){
    val in = InPort("In", RobotisFeedback(id,0,0,0,0,0,0))
    val position = OutPort("position", 0.0)
    val speed = OutPort("speed",0.0)
    val sload = OutPort("sload",0.0)
    val voltage = OutPort("voltage",0.0)
    val temperature = OutPort("temp",0.0)
    val moving = OutPort("moving",false)
    override def act{
      position.v = in.v.position  
      speed.v = (in.v.speed / 1023.0) * 144
      sload.v = in.v.sload 
      voltage.v = (in.v.voltage /10.0) 
      temperature.v = in.v.temperature 
      moving.v = in.v.moving ==1
    }
  }
  driver.feedbackSource(id) connect demux.in
  
}