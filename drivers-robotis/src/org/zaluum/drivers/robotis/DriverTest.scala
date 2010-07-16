package org.zaluum.drivers.robotis

object DriverTest {

  def main(args: Array[String]): Unit = { 
     val a = new AX12("/dev/ttyUSB0")
     a.start
     //a.enterTossMode();
     //Thread.sleep(200)
     println("pinging")
     a.ping(12)
     println("pinged")
     a.set_servo_position(12, 500)
     /*(id:Int, torqueEnable: Boolean, led:Boolean, cwmargin : Int, ccwmargin: Int, cwslope : Int, ccwslope: Int,
      goalPosition:Int, movingSpeed:Int, torqueLimit:Int)*/
     val l = List(
         RobotisCommand(12, true,true, 0,0,1,1,100,130,100)
         )
     Thread.sleep(1000)
     a.set_multi_servo_command(l);
//     a.set_servo_position(10, 600)
  }

}