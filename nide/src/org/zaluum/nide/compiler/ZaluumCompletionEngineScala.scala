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
object ZaluumCompletionEngineScala {
  def findBySignature(cud: ZaluumCompilationUnitDeclaration,
                      zcs: ZaluumClassScope,
                      c: ClassJavaType, selector: String, signature: String, static: Boolean) = {
    allMethods(engineFor(cud), zcs, c.binding, static) find { m ⇒
      m.selector.mkString == selector &&
        m.signature().mkString == signature
    }
  }
  def findField(cud: ZaluumCompilationUnitDeclaration,
                zcs: ZaluumClassScope,
                r: ReferenceBinding, name: String, static: Boolean) = {
    allFields(engineFor(cud), zcs, r, static) find { f ⇒
      f.name.mkString == name
    }
  }
  def findConstructor(cud: ZaluumCompilationUnitDeclaration,
                      zcs: ZaluumClassScope,
                      r: ReferenceBinding, signature: String) = 
    allConstructors(engineFor(cud), zcs, r) find { _.signature().mkString == signature }

  def allFields(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, binding: ReferenceBinding, static: Boolean): List[FieldBinding] =
    rewrapToList(engine.findAllFields(binding, zcs, static))
  def allMethods(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, c: ReferenceBinding, static: Boolean): List[MethodBinding] =
    rewrapToList(engine.findAllMethods(c, zcs, static))
  def allConstructors(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, c: ReferenceBinding): List[MethodBinding] =
    engine.findAllConstructors(c, zcs).toList

  def engineFor(cud: ZaluumCompilationUnitDeclaration): ZaluumCompletionEngine = {
    val lookup = cud.zaluumScope.environment
    new ZaluumCompletionEngine(lookup)
  }

  def engineForVs(vs: ValSymbol): ZaluumCompletionEngine =
    engineFor(vs.owner.template.asInstanceOf[BoxTypeSymbol].javaScope.compilationUnitScope
      .asInstanceOf[ZaluumCompilationUnitScope].cud)

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
