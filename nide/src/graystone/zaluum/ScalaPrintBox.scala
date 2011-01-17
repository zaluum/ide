package graystone.zaluum

import org.zaluum.nide.java.{In,Box}

@Box(image="graystone/zaluum/print.png")
class ScalaPrintBox {
  @In(x=0,y=15) var a : Double = 0.0
  def apply(){
    println("scala print " + a)
  }
}