package org.zaluum.example
import org.zaluum.runtime._
import org.zaluum.drivers.local.ConsoleDriver
import org.zaluum.drivers.local.SWTKeyboardDriver
import org.zaluum.drivers.robotis.RobotisDriver
import scala.collection.mutable.{Map,Set}

trait RobotisExampleIO {
  val m1PositionSource: Source[Int]
  val m2PositionSink: Sink[Int]
}

class RealRobotisExample(setup:Setup) extends RobotisExampleIO{
  val driver = new RobotisDriver(setup,refresh=100)
  val m1PositionSource = driver.positionSourceForId(1) 
  val m2PositionSink = driver.positionSinkForId(2) 
}
class KeyboardRobotisExample(setup:Setup) extends RobotisExampleIO{
  val keyboard = new SWTKeyboardDriver(setup)
  val console = new ConsoleDriver(setup)
  val m1PositionSource = keyboard.getKeySource('a',100,200)
  val m2PositionSink = console.getConsoleSink("m2Position")
}

class RobotisExample extends ModelBuilder{  
  class SignChanger(name:String,parent:ComposedBox) extends Box(name,parent) {
    val in = InPort("in",0)
    val out = OutPort("out",0)
    override def act { out.v = -in.v}
  }
  def create(setup:Setup) = {
    var io  =  new KeyboardRobotisExample(setup)
    new MainBox {
      val inverter = new SignChanger("SignChanger",this)
      io.m1PositionSource connect inverter.in
      inverter.out connect io.m2PositionSink
    }
  }
}
