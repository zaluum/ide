package graystone.zaluum

import graystone.zaluum.annotations.In
import graystone.zaluum.annotations.Box

@Box
class ScalaPrintBox {
  @In var a : Double = 0.0
  def apply(){
    println("scala print " + a)
  }
}