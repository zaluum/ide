package org.zaluum.nide.zge

import org.zaluum.nide.newcompiler._
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.ColorConstants
import org.zaluum.nide.newcompiler.Type
object Colorizer {
  def color(t:Symbol)  : Color= t match {
    case NoSymbol => ColorConstants.gray
    case a => ColorConstants.blue
  }
}