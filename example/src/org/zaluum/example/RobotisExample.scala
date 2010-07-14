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
class SourceBox(name:String,parent:ComposedBox,source:Source[Double]) extends Box(name,parent) {
  val out = OutPort("out",0.0)
  override def init(process:Process){
    source.suscribe(this)
  }
  override def act {
    out.v = source.v
  }
}
class RobotisExample extends ModelBuilder{  
  
  def create(setup:Setup) = {
    var io  =  new RealRobotisExample(setup)
    
    new MainBox {
      val sourcem1 = new SourceBox("Source1",this,io.m1PositionSource)
      val sourcem2 = new SourceBox("Source2",this,io.m2PositionSource)
      val sourcem3 = new SourceBox("Source3",this,io.m3PositionSource)
      
      val m1 = new AX12Motor("M1",this,io.driver,8)
      val m2 = new AX12Motor("M2",this,io.driver,10)
      val m3 = new AX12Motor("M3",this,io.driver,12)
      
      val torque = new Const ("one", this, 0)
      
      sourcem1.out connect m1.goalPosition 
      sourcem2.out connect m2.goalPosition 
      sourcem3.out connect m3.goalPosition 
      
      
    //  torque.out connect io.m1TorqueEnabledSink
    }
  }
}
