package graystone.zaluum

import graystone.zaluum.annotations.In
import graystone.zaluum.annotations.Box

@Box(image="graystone/zaluum/print.png")
class ScalaPrintBox {
  @In(x=0,y=15) var a : Double = 0.0
  def apply(){
    println("scala print " + a)
  }
}