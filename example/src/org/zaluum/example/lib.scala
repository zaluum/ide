package org.zaluum.example;
import org.zaluum.runtime.{Box, InPort,OutPort,ComposedBox,Process}
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

class AX12Motor(name:String,parent:ComposedBox,driver : RobotisDriver, id:Int) extends Box(name,parent) {
  val position = OutPort("position", 0.0)
  val speed = OutPort("speed",0.0)
  val sload = OutPort("sload",0.0)
  val voltage = OutPort("voltage",0.0)
  val temperature = OutPort("temp",0.0)
  val moving = OutPort("moving",false)
  
  val torqueEnable = InPort("torque",false)
  val led = InPort("led", false)
  val cwMargin = InPort("CWMargin",0.0)
  val ccwMargin = InPort("CCWMargin",0.0)
  val cwSlope = InPort("CWSlope",0.0)
  val ccwSlope = InPort("CCWSlope",0.0)
  val goalPosition = InPort("goalPosition",0.0)
  val goalSpeed = InPort("goalSpeed",0.0)
  val torqueLimit = InPort("torqueLimit",0.0)
  val punch = InPort("punch",0.0)
  override def init(process:Process) {
    driver.feedbackSource(id).suscribe(this)
  }
  override def act {
    val v = driver.feedbackSource(id).v
    if (v!=null){
      position.v = v.position  
      speed.v = (v.speed / 1023.0) * 144
      sload.v = v.sload 
      voltage.v = (v.voltage /10.0) 
      temperature.v = v.temperature 
      moving.v = v.moving ==1
    }
    driver.positionSinkForId(id).write(goalPosition.v.asInstanceOf[Int])
  }
  
}