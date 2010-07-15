package org.zaluum.drivers.robotis

object CM5Setup {
  def enterTossMode(device : String = "/dev/ttyUSB0"){
    println("sending toss mode command t")
    val a = new AX12(device,57600)
    a.start
    a.enterTossMode()
    a.stop
  }
  def main(args:Array[String]){
    enterTossMode()
  }
}