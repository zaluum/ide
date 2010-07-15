package org.zaluum.example
import org.zaluum.runtime._
import org.zaluum.drivers.local.ConsoleDriver
import org.zaluum.drivers.local.SWTKeyboardDriver
import org.zaluum.drivers.robotis.RobotisDriver
import scala.collection.mutable.{Map,Set}

class RealRobotisExample(setup:Setup) {

  val driver = new RobotisDriver(setup,refresh=100)
  val keyboard = new SWTKeyboardDriver(setup)
  val console =  new ConsoleDriver(setup)
  val m1PositionSource = keyboard.getKeySource('w', 500, 700) 
  val m2PositionSource = keyboard.getKeySource('a', 500, 700)
  val m3PositionSource = keyboard.getKeySource('s', 500, 700)
  val consoleSink = console.getConsoleSink("time");
  //val m1TorqueEnabledSink = driver.torqueSinkForId(10)
  val m1PositionSink = driver.positionSinkForId(3)
  val m2PositionSink = driver.positionSinkForId(10)
  val m3PositionSink = driver.positionSinkForId(12)
}

class RobotisExample extends ModelBuilder{  
  
  def create(setup:Setup) = {
    var io  =  new RealRobotisExample(setup)
    
    new MainBox {
      val sourcem1 = new SourceBox("Source1",this,io.m1PositionSource)
      sourcem1.pos = (0,0,50,50)
      val sourcem2 = new SourceBox("Source2",this,io.m2PositionSource)
      sourcem2.pos = (0,100,50,50)

      val sourcem3 = new SourceBox("Source3",this,io.m3PositionSource)
      sourcem3.pos = (0,200,50,50)

      val m1 = new AX12Motor("M1",this,io.driver,3)
      m1.pos = (200,0, 70,90)
      val m2 = new AX12Motor("M2",this,io.driver,10)
      m2.pos = (200,100, 70,90)
      val m3 = new AX12Motor("M3",this,io.driver,12)
      m3.pos = (200,200, 70,90)
      val t = new Time("time",this)
      val sin = new Op("sin",this)( (x : Int) => 
        math.abs((math.sin(x/500.0)+1) * 512))
      val c = new SinkBox("timeSink",this,io.consoleSink)
      //t.out connect c.in
      t.out connect sin.in
      val torque = new Const ("one", this, 0)
      sin.out connect m1.goalPosition
      //sourcem1.out connect m1.goalPosition 
      sourcem2.out connect m2.goalPosition 
      sourcem3.out connect m3.goalPosition 
      
      
    //  torque.out connect io.m1TorqueEnabledSink
    }
  }
}
