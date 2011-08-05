package org.zaluum.nide.eclipse.integration

object ContentTypeUtils {
  def isZaluumLikeFileName(str: Array[Char]): Boolean = isZaluumLikeFileName(str.mkString)
  def isZaluumLikeFileName(str: String): Boolean = {
    str.endsWith("zaluum")
  }
}