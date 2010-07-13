package org.zaluum.drivers.robotis

object DriverTest {

  def main(args: Array[String]): Unit = { 
     val a = new AX12("/dev/ttyUSB0")
     a.start
     //a.enterTossMode();
     //Thread.sleep(200)
     println("pinging")
     //a.ping(10)
     println("pinged")
     a.set_servo_position(10, 600)
  }

}