package org.zaluum.nide.zge

import org.zaluum.nide.compiler._
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.ColorConstants
import org.zaluum.nide.compiler.Type
object Colorizer {
  def color(t:Symbol)  : Color= t match {
    case NoSymbol => ColorConstants.gray
    case a => ColorConstants.blue
  }
}