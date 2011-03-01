package org.zaluum.nide.zge

import org.zaluum.nide.compiler._
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.ColorConstants
import org.zaluum.nide.compiler.Type
object Colorizer {
  def color(t:Symbol)  : Color= t match {
    case t: Type => t.name match {
      case Name("double") => ColorConstants.orange
      case Name("boolean") => ColorConstants.red
      case _ => ColorConstants.lightBlue
    }
    case a => ColorConstants.gray
  }
}