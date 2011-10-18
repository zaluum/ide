package org.zaluum.nide.zge

object Snap {
  def snap(movLow: Int, movHigh: Int, fixLow: Int, fixHigh: Int, current: Int, minDist: Int) = {
    val h = movHigh - movLow
    val movCenter = movLow + (h / 2)
    val fixCenter = fixLow + ((fixHigh - fixLow) / 2)
    var dist = Int.MaxValue
      def better(a: Int, b: Int) = {
        val me = math.abs(a - b)
        if (me <= minDist && me < dist) {
          dist = me
          true
        } else false
      }
    var res = current
    if (better(movLow, fixLow)) res = fixLow
    if (better(movHigh, fixHigh)) res = fixHigh - h
    if (better(movLow, fixHigh)) res = fixHigh
    if (better(movHigh, fixLow)) res = fixLow - h
    if (better(movCenter, fixCenter)) res = fixCenter - (h / 2)
    res
  }

}