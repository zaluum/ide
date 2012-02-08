package org.zaluum.nide.compiler
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitScope
import org.eclipse.jdt.internal.compiler.util.ObjectVector
import scala.collection.JavaConversions._
import org.zaluum.nide.eclipse.integration.model.ZaluumTypeDeclaration
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope

trait ClassJavaTypeFinder {
  self: ClassJavaType ⇒
  def findBySignature(selector: String, signature: String, static: Boolean, scope: ZaluumClassScope) = {
    allMethods(static, scope) find { m ⇒
      m.selector.mkString == selector &&
        m.signature().mkString == signature
    }
  }
  def findMatchingConstructors(arity: Int,
                               incomingTypes: Seq[Option[JavaType]],
                               scope: ZaluumClassScope) = {
    allConstructors(scope) filter { m ⇒
      ((m.parameters == null && arity == 0) || m.parameters.length == arity) &&
        matchesParameters(m, incomingTypes, scope)
    }
  }
  def findMatchingMethods(selector: String,
                          arity: Int,
                          incomingTypes: Seq[Option[JavaType]],
                          static: Boolean,
                          scope: ZaluumClassScope): List[MethodBinding] = {
    allMethods(static, scope) filter { m ⇒
      m.selector.mkString == selector &&
        matchesArityAndParameters(m, arity, incomingTypes)
    }
  }
  def matchesArityAndParameters(m: MethodBinding, arity: Int, incomingTypes: Seq[Option[JavaType]]): Boolean = {
    ((m.parameters == null && arity == 0) || m.parameters.length == arity) &&
      matchesParameters(m, incomingTypes, scope)
  }
  def matchesParameters(m: MethodBinding, incomingTypes: Seq[Option[JavaType]], scope: ZaluumClassScope): Boolean = {
    if (m.parameters == null) { true }
    else {
      m.parameters.zip(incomingTypes).forall {
        case (t, ot) ⇒
          ot match {
            case Some(incomingT) ⇒
              CheckConnections.checkAssignmentPossible(Some(incomingT), scope.getJavaType(t), scope)
            case None ⇒ true
          }
      }
    }
  }

  def findField(name: String, static: Boolean, scope: ZaluumClassScope) = {
    allFields(static, scope) find { f ⇒
      f.name.mkString == name
    }
  }
  //def findConstructor(c: ClassJavaType, signature: String) =
  //allConstructors(engineFor(c.scope), c.scope, c.binding) find { _.signature().mkString == signature }
  def allFields(static: Boolean, scope: ZaluumClassScope): List[FieldBinding] =
    rewrapToList(engineFor(scope).findAllFields(binding, scope, static))
  def allMethods(static: Boolean, scope: ZaluumClassScope): List[MethodBinding] =
    rewrapToList(engineFor(scope).findAllMethods(binding, scope, static))
  def allConstructors(scope: ZaluumClassScope): List[MethodBinding] =
    engineFor(scope).findAllConstructors(binding, scope).toList
  private def engineFor(zcs: ZaluumClassScope): ZaluumCompletionEngine =
    new ZaluumCompletionEngine(zcs.environment)

  private def rewrapToList[A](ov: ObjectVector): List[A] = {
    var l = List[A]()
    for (i ← 0 until ov.size) {
      val o = ov.elementAt(i).asInstanceOf[Array[_]]
      val obj = o(0).asInstanceOf[A]
      l ::= obj
    }
    l
  }
}
