package org.zaluum.example;
import org.zaluum.runtime.{Box, InPort,OutPort,ComposedBox,Process, Sink,Source}
import org.zaluum.drivers.robotis.{RobotisDriver, RobotisFeedback}
class Op[A:Manifest,B:Manifest](name:String,parent:ComposedBox)(op : A => B) extends Box(name,parent) {
  val in = InPort[A]("in")
  val out = OutPort[B]("out")
  override def act { out.v = op(in.v) }
}
class Const[A:Manifest](name : String, parent:ComposedBox, const : A) extends Box(name,parent) {
  val  out = OutPort[A]("out",const)
}
class Forward[A:Manifest](name:String,parent:ComposedBox) extends Op[A,A](name,parent)({x=>x})

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
class SourceBox[A:Manifest](name:String,parent:ComposedBox,source:Source[A]) extends Box(name,parent) {
  val out = OutPort[A]("out")
  override def init(process:Process){
    source.suscribe(this)
  }
  override def act {
    out.v = source.v
  }
}
class SinkBox[A:Manifest](name:String,parent:ComposedBox,sink:Sink[A]) extends Box(name,parent) {
  val in = InPort[A]("in")
  override def act {
    sink.write(in.v);
  }
}

class Time(name:String,parent:ComposedBox) extends Box(name,parent){
  val out = OutPort[Long]("out")
  override def act (process:Process) {
    out.v = (process.time.nowNano / 1000000)
    process.time .queue(this, 10)
  }
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