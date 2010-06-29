package org.zaluum.runtime

object Util {
  def checkAssignable[T](x: Any,mf: Manifest[T]) = {
    mf == manifest[Any] || {
      def primitive(c: Class[_]) = mf == manifest[AnyVal] || mf.erasure == c
      x match {
        case _: Byte    => primitive(classOf[Byte])
        case _: Short   => primitive(classOf[Short])
        case _: Int     => primitive(classOf[Int])
        case _: Long    => primitive(classOf[Long])
        case _: Float   => primitive(classOf[Float])
        case _: Double  => primitive(classOf[Double])
        case _: Char    => primitive(classOf[Char])
        case _: Boolean => primitive(classOf[Boolean])
        case _: Unit    => primitive(classOf[Unit])
        case null => !(mf.erasure.isPrimitive || classOf[NotNull].isAssignableFrom(mf.erasure))
        case x:AnyRef => mf.erasure.isAssignableFrom(x.getClass)
     }
    }
  }
}