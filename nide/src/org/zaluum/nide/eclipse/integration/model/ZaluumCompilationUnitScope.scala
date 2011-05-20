package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.Scope
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.compiler._
import org.eclipse.jdt.internal.compiler.lookup.BinaryTypeBinding
import org.zaluum.annotation.Box
import JDTInternalUtils._
import scala.collection.mutable.Map
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
class ZaluumCompilationUnitScope(cud: ZaluumCompilationUnitDeclaration, lookupEnvironment: LookupEnvironment) extends CompilationUnitScope(cud, lookupEnvironment) {
  override protected def buildClassScope(parent: Scope, typeDecl: TypeDeclaration) = {
    new ZaluumClassScope(parent, typeDecl)
  }

  val cache = Map[Name, BoxTypeSymbol]()
  val cacheJava = Map[TypeBinding, JavaType]()
  def getJavaType(name: Name): Option[JavaType] = {
    val tpe = if (name.str.contains(".")) {
      val compoundName = stringToA(name.str)
      getType(compoundName, compoundName.length)
    } else {
      getType(name.str.toCharArray)
    }
    getJavaType(tpe)
  }
  def getJavaType(tpe: TypeBinding): Option[JavaType] = {
    cacheJava.get(tpe).orElse {
      val jtpe = tpe match {
        case r: ReferenceBinding ⇒ Some(new JavaType(cud.JDTScope, Name(aToString(r.compoundName))))
        case b: BaseTypeBinding ⇒ b.simpleName.mkString match {
          case "byte" ⇒ Some(cud.JDTScope.primitives.byte)
          case "short" ⇒ Some(cud.JDTScope.primitives.short)
          case "int" ⇒ Some(cud.JDTScope.primitives.int)
          case "long" ⇒ Some(cud.JDTScope.primitives.long)
          case "float" ⇒ Some(cud.JDTScope.primitives.float)
          case "double" ⇒ Some(cud.JDTScope.primitives.double)
          case "boolean" ⇒ Some(cud.JDTScope.primitives.boolean)
          case "char" ⇒ Some(cud.JDTScope.primitives.char)
          case _ ⇒ None
        }
        case _ ⇒ None
      }
      jtpe foreach { j ⇒
        cacheJava += (tpe -> j)
      }
      jtpe
    }
  }
  def getBoxType(name: Name): Option[BoxTypeSymbol] = {
    cache.get(name).orElse {
      val compoundName = stringToA(name.str)
      getType(compoundName, compoundName.length) match {
        case r: ReferenceBinding ⇒
          def isBox = {
            r.getAnnotations exists { a ⇒
              val aName = aToString(a.getAnnotationType.compoundName)
              classOf[Box].getName == aName
            }
          }
          if (isBox) {
            val sper = aToString(r.superclass().compoundName)
            val sperO = sper match {
              case null ⇒ None
              case "java.lang.Object" ⇒ None
              case other ⇒ Some(Name(other))
            }
            val bs = new BoxTypeSymbol(cud.a.global.root, name, sperO, None, None, r.isAbstract)
            bs.scope = cud.a.global
            for (f ← r.fields()) {
              val fname = f.name.mkString
              def hasAnnotation(c: Class[_]) = f.getAnnotations.exists { a ⇒
                aToString(a.getAnnotationType.compoundName) == c.getName
              }
              def createPort(in: Boolean) = {
                val port = new PortSymbol(bs, Name(fname), Point(0, 0), if (in) In else Out)
                port.tpe = getJavaType(f.`type`).getOrElse(NoSymbol)
                bs.enter(port)
              }
              if (hasAnnotation(classOf[org.zaluum.annotation.In])) createPort(true)
              else if (hasAnnotation(classOf[org.zaluum.annotation.Out])) createPort(false)
              if (fname == "_widget") {
                f.`type` match {
                  case r: ReferenceBinding ⇒
                    bs.visualClass = Some(Name(aToString(r.compoundName)))
                  case _ ⇒
                }
              }
            }
            for (m ← r.availableMethods) {
              val mName = m.selector.mkString
              if (m.isConstructor && m.isPublic) {
                  val params = for (p ← m.parameters) yield {
                    val s = if (m.selector == null) "?" else m.selector.mkString
                    val ps = new ParamSymbol(bs, Name(s))
                    ps.tpe = getJavaType(p).getOrElse(NoSymbol)
                    ps
                  }
                  bs.constructors = new Constructor(bs, params.toList) :: bs.constructors
              } else {
                if (mName.startsWith("set") && m.parameters.size == 1 && m.returnType == TypeBinding.VOID) {
                  getJavaType(m.parameters.head) foreach { ptpe ⇒
                    val p = new ParamSymbol(bs, Name(mName))
                    p.tpe = ptpe
                    bs.enter(p)
                  }
                }
              }
            }
            if (bs.constructors.isEmpty)
              bs.constructors = List(new Constructor(bs, List())) 
            cache += (name -> bs)
            Some(bs)
          } else {
           // println(name + " is not a box")
            None
          }
        case _ ⇒ None
      }
    }
  }
}