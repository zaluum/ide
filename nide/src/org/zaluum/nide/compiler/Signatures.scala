package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.zaluum.nide.utils.JDTUtils

object Signatures {
  val MethodSignature = """(.+)(\(.*)""".r
  def findConstructor(c: ClassJavaType, scope: ZaluumClassScope, methoduid: String, static: Boolean): Option[MethodBinding] = {
    val engine = ZaluumCompletionEngineScala.engineFor(scope)
    c.binding match {
      case r: ReferenceBinding ⇒
        ZaluumCompletionEngineScala.allConstructors(engine, scope, r).find {
          matches(_, methoduid)
        }
      case _ ⇒ None
    }
  }
  def findMethod(c: ClassJavaType, scope: ZaluumClassScope, methoduid: String, static: Boolean): Option[MethodBinding] = {
    val engine = ZaluumCompletionEngineScala.engineFor(scope)
    c.binding match {
      case r: ReferenceBinding ⇒
        ZaluumCompletionEngineScala.allMethods(engine, scope, r, static).find {
          matches(_, methoduid)
        }
      case _ ⇒ None
    }
  }

  def findField(c: ClassJavaType, scope: ZaluumClassScope, fieldName: String, static: Boolean) = {
    val engine = ZaluumCompletionEngineScala.engineFor(scope)
    c.binding match {
      case r: ReferenceBinding ⇒
        ZaluumCompletionEngineScala.allFields(engine, scope, r, static).find {
          matches(_, fieldName)
        }
      case _ ⇒ None
    }
  }
  def methodUID(m: MethodBinding) = m.selector.mkString + m.signature.mkString
  def matches(m: MethodBinding, pmethodUID: String): Boolean =
    methodUID(m) == pmethodUID
  def matches(f: FieldBinding, name: String): Boolean =
    f.name.mkString == name
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
          case Some(cl: ClassJavaType) ⇒ findMethod(cl, scope, muid, true)
          case _                       ⇒ None
        }
      case _ ⇒ None
    }
  }
}