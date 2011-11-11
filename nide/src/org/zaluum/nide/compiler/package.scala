package org.zaluum.nide
package object compiler {
  type TreePF = PartialFunction[Tree, Tree]
  object Int {
    def unapply(s: String): Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: java.lang.NumberFormatException ⇒ None
    }
  }
}
