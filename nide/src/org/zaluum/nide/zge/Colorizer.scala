package org.zaluum.nide.zge

import org.eclipse.draw2d.ColorConstants
import org.eclipse.swt.graphics.Color
import org.zaluum.nide.compiler.ArrayType
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Symbol
import org.zaluum.nide.compiler.JavaType
object Colorizer {
  def color(t: Option[JavaType]): Color = t match {
    case Some(tp) ⇒ tp match {
      case a: ArrayType ⇒ ColorConstants.green
      case t: JavaType ⇒ t.name match {
        case Name("byte")    ⇒ ColorConstants.cyan
        case Name("short")   ⇒ ColorConstants.lightBlue
        case Name("int")     ⇒ ColorConstants.blue
        case Name("long")    ⇒ ColorConstants.darkBlue
        case Name("float")   ⇒ ColorConstants.yellow
        case Name("double")  ⇒ ColorConstants.orange
        case Name("boolean") ⇒ ColorConstants.red
        case Name("char")    ⇒ ColorConstants.green
        case a               ⇒ ColorConstants.black
      }
      case a ⇒ ColorConstants.gray
    }
    case None ⇒ ColorConstants.gray
  }
}