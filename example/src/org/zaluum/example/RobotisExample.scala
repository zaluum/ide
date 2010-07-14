package org.zaluum.example
import org.zaluum.runtime._
import org.zaluum.drivers.local.ConsoleDriver
import org.zaluum.drivers.local.SWTKeyboardDriver
import org.zaluum.drivers.robotis.RobotisDriver
import scala.collection.mutable.{Map,Set}

class RealRobotisExample(setup:Setup) {

  val driver = new RobotisDriver(setup,refresh=100)
  val keyboard = new SWTKeyboardDriver(setup)
  val m1PositionSource = keyboard.getKeySource('w', 500, 700) 
  val m2PositionSource = keyboard.getKeySource('a', 500, 700)
  val m3PositionSource = keyboard.getKeySource('s', 500, 700)
  
  //val m1TorqueEnabledSink = driver.torqueSinkForId(10)
  val m1PositionSink = driver.positionSinkForId(8)
  val m2PositionSink = driver.positionSinkForId(10)
  val m3PositionSink = driver.positionSinkForId(12)
}

class RobotisExample extends ModelBuilder{  
  
  def create(setup:Setup) = {
    var io  =  new RealRobotisExample(setup)
    
    new MainBox {
      val forward1 = new Forward ("F1",this)
      val forward2 = new Forward("F2",this)
      val forward3 = new Forward("F3",this)
      val m1 = new AX12Motor("M1",this,io.driver,8)
      val torque = new Const ("one", this, 0)
      
    //  torque.out connect io.m1TorqueEnabledSink
 
      io.m1PositionSource connect forward1.in
      io.m2PositionSource connect forward2.in
      io.m3PositionSource connect forward3.in

      forward1.out connect io.m1PositionSink
      forward2.out connect io.m2PositionSink
      forward3.out connect io.m3PositionSink 
    }
  }
}
