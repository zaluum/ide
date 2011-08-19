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
object ZaluumCompletionEngineScala {
  def findBySignature(ztd: ZaluumTypeDeclaration,
                      zcs: ZaluumClassScope,
                      c: ClassJavaType, selector: String, signature: String, static: Boolean) = {
    allMethods(engineFor(ztd), zcs, c.binding, static) find { m ⇒
      m.selector.mkString == selector &&
        m.signature().mkString == signature
    }
  }
  def findField(ztd: ZaluumTypeDeclaration,
                zcs: ZaluumClassScope,
                r: ReferenceBinding, name: String, static: Boolean) = {
    allFields(engineFor(ztd), zcs, r, static) find { f ⇒
      f.name.mkString == name
    }
  }
  def findConstructor(ztd: ZaluumTypeDeclaration,
                      zcs: ZaluumClassScope,
                      r: ReferenceBinding, signature: String) = 
    allConstructors(engineFor(ztd), zcs, r) find { _.signature().mkString == signature }

  def allFields(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, binding: ReferenceBinding, static: Boolean): List[FieldBinding] =
    rewrapToList(engine.findAllFields(binding, zcs, static))
  def allMethods(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, c: ReferenceBinding, static: Boolean): List[MethodBinding] =
    rewrapToList(engine.findAllMethods(c, zcs, static))
  def allConstructors(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, c: ReferenceBinding): List[MethodBinding] =
    engine.findAllConstructors(c, zcs).toList
  def engineFor(zcs: ZaluumClassScope) : ZaluumCompletionEngine = 
    new ZaluumCompletionEngine(zcs.environment)
  def engineFor(ztd: ZaluumTypeDeclaration): ZaluumCompletionEngine = 
    engineFor(ztd.zaluumScope)

  def engineForVs(vs: ValSymbol): ZaluumCompletionEngine =
    engineFor(vs.owner.template.asInstanceOf[BoxTypeSymbol].javaScope.ztd)

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
