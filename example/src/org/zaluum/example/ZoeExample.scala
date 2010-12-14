package org.zaluum.example

import graystone.ZoeDriver
import org.zaluum.drivers.local.{SWTKeyboardDriver, ConsoleDriver}
import org.zaluum.drivers.robotis.RobotisDriver
import org.zaluum.runtime._

class ZoeExample extends ModelBuilder{  
  
  def create(setup:Setup) = {
    //val driver = new ZoeDriver(setup)
    new MainBox {
      //val sourcem1 = new SourceBox("Source1",this,driver.toey)
      /*val sourcem2 = new SourceBox("Source2",this,io.m2PositionSource)
      val sourcem3 = new SourceBox("Source3",this,io.m3PositionSource)

      val m1 = new AX12Motor("M1",this,io.driver,8)
      val m2 = new AX12Motor("M2",this,io.driver,10)
      val m3 = new AX12Motor("M3",this,io.driver,12)
      
      val t = new Time("time",this)
      val sin1 = new Op("sin1",this)( (x:Long) =>  ((math.sin(x/500.0)+1.0) * 20.0))
      val sin2 = new Op("sin2",this)( (x:Long) =>  ((math.sin(x/400.0)+1.0) * 20.0))
      val sin3= new Op("sin3",this)( (x:Long) =>  ((math.sin(x/300.0)+1.0) * 20.0))
      
      t.out connect sin1.in
      t.out connect sin2.in
      t.out connect sin3.in
      sin1.out connect m1.goalPosition
      sin2.out connect m2.goalPosition
      sin3.out connect m3.goalPosition

    //  sourcem1.out connect m1.goalPosition 
    //  sourcem2.out connect m2.goalPosition 
    //  sourcem3.out connect m3.goalPosition 
      
      sourcem1.pos = (  0,  0, 50, 50)
      sourcem2.pos = (  0,200, 50, 50)
      sourcem3.pos = (  0,400, 50, 50)
      m1.pos       = (300,  0,150,150)
      m2.pos       = (300,200,150,150)
      m3.pos       = (300,400,150,150)*/
    //  torque.out connect io.m1TorqueEnabledSink
    //  val c = new SinkBox("timeSink",this,io.consoleSink)
    }
  }
}
