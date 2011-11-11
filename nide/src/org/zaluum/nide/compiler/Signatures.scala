package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.core.IMethod
import org.zaluum.nide.utils.JDTUtils

object Signatures {
  val MethodSignature = """(.+)(\(.*)""".r
  private def findMethod2(c: ClassJavaType, scope: ZaluumClassScope, methoduid: String, static: Boolean): Option[MethodBinding] = {
    c.allMethods(static, scope).find {
      matches(_, methoduid)
    }
  }

  private def methodUID(m: MethodBinding) = m.selector.mkString + m.signature.mkString
  private def matches(m: MethodBinding, pmethodUID: String): Boolean =
    methodUID(m) == pmethodUID
  private def matches(f: FieldBinding, name: String): Boolean =
    f.name.mkString == name
  // method and arity
  private val MethodAndArityRegexp = """(.*)\|(.*)""".r
  object MethodAndArity {
    def unapply(str: String): Option[(String, Int)] = methodAndAritySplit(str)
  }
  def methodAndArity(m: MethodBinding) = m.selector.mkString + "|" + m.parameters.length
  def methodAndAritySplit(str: String): Option[(String, Int)] =
    str match {
      case MethodAndArityRegexp(selector, Int(num)) ⇒ Some(selector, num)
      case _                                        ⇒ None
    }
  // static method
  private val StaticMethodUIDRegexp = """(.*)#(.*)""".r

  def staticMethodUIDSplit(staticUID: String): (String, String) = {
    staticUID match {
      case StaticMethodUIDRegexp(cl, muid) ⇒ (cl, muid)
      case _                               ⇒ (staticUID, "")
    }
  }
  def findStaticMethodUID(staticUID: String, scope: ZaluumClassScope): Option[MethodBinding] = {
    staticUID match {
      case StaticMethodUIDRegexp(clName, muid) ⇒
        scope.lookupType(Name(clName)) match {
          case Some(cl: ClassJavaType) ⇒ findMethod2(cl, scope, muid, true)
          case _                       ⇒ None
        }
      case _ ⇒ None
    }
  }
}