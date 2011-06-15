package org.zaluum.nide.zge

import org.zaluum.nide.compiler._
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.ColorConstants
import org.zaluum.nide.compiler.Type
object Colorizer {
  def color(t:Symbol)  : Color= t match {
    case a:ArrayType => ColorConstants.green
    case t: Type => t.name match {
      case Name("byte") => ColorConstants.cyan
      case Name("short") => ColorConstants.lightBlue
      case Name("int") => ColorConstants.blue
      case Name("long") => ColorConstants.darkBlue
      case Name("float") => ColorConstants.yellow
      case Name("double") => ColorConstants.orange
      case Name("boolean") => ColorConstants.red
      case Name("char") => ColorConstants.green
      case a => ColorConstants.black      
    }
    case a => ColorConstants.gray
  }
}