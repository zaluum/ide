package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.ClassScope
import org.eclipse.jdt.internal.compiler.lookup.Scope
import scala.collection.mutable.Map
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.MissingTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.ProblemReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.Scope
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.lookup.UnresolvedReferenceBinding
import org.zaluum.nide.compiler.ArrayType
import org.zaluum.nide.compiler.ClassJavaType
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.JavaType
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.ParamDecl
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.PortSymbol
import org.zaluum.nide.compiler.PrimitiveJavaType
import org.zaluum.nide.compiler.{ Scope ⇒ ZScope }
import org.zaluum.nide.compiler.primitives
import org.zaluum.nide.utils.JDTUtils.aToString
import org.zaluum.nide.utils.JDTUtils.stringToA
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.eclipse.jdt.internal.compiler.impl.StringConstant
import org.zaluum.nide.compiler.PortDir
import org.eclipse.jdt.internal.compiler.lookup.AnnotationBinding
import org.zaluum.nide.utils.MethodBindingUtils
import org.zaluum.nide.utils.JDTUtils

class ZaluumClassScope(parent: Scope, typeDecl: TypeDeclaration) extends ClassScope(parent, typeDecl) with ZScope {
  override protected def buildClassScope(parent: Scope, typeDecl: TypeDeclaration) = {
    new ZaluumClassScope(parent, typeDecl)
  }
  def ztd = referenceContext.asInstanceOf[ZaluumTypeDeclaration]
  private val cacheJava = Map[TypeBinding, ClassJavaType]()
  def name = Name("root")
  def owner = null
  def getBoxedType(p: PrimitiveJavaType): Option[JavaType] = lookupType(p.boxedName)
  def getZJavaLangString = lookupType(Name("java.lang.String")).get
  def javaScope: ZaluumClassScope = this
  def lookupType(name: Name): Option[JavaType] = {
    val arr = name.asArray
    if (arr.isDefined) {
      val (leafname, dim) = arr.get
      lookupType(leafname) map { t ⇒
        getArrayType(t, dim)
      }
    } else {
      try {
        val tpe =
          if (name.str.contains(".")) {
            val compoundName = stringToA(name.str)
            getType(compoundName, compoundName.length)
          } else {
            getType(name.str.toCharArray)
          }
        getJavaType(tpe)
      } catch { case e ⇒ e.printStackTrace(); None }
    }
  }
  def getArrayType(t: JavaType, dim: Int): ArrayType = {
    val bind = createArrayType(t.binding, dim)
    val a = new ArrayType(t, dim, bind)
    a
  }
  def getJavaType(tpe: TypeBinding): Option[JavaType] = {
    tpe match {
      case m: MissingTypeBinding         ⇒ None
      case p: ProblemReferenceBinding    ⇒ None
      case u: UnresolvedReferenceBinding ⇒ None
      case r: ReferenceBinding ⇒
        val tpe = environment.convertToRawType(r, false).asInstanceOf[ReferenceBinding]
        if (tpe != null) {
          Some(cacheJava.getOrElseUpdate(tpe, create(tpe)))
        } else None
      case b: BaseTypeBinding ⇒
        Some(b.simpleName.mkString match {
          case "byte"    ⇒ primitives.Byte
          case "short"   ⇒ primitives.Short
          case "int"     ⇒ primitives.Int
          case "long"    ⇒ primitives.Long
          case "float"   ⇒ primitives.Float
          case "double"  ⇒ primitives.Double
          case "boolean" ⇒ primitives.Boolean
          case "char"    ⇒ primitives.Char
        })
      case a: ArrayBinding ⇒
        getJavaType(a.leafComponentType) map { leaf ⇒
          new ArrayType(leaf, a.dimensions, a)
        }
    }
  }
  protected def create(r: ReferenceBinding) = new ClassJavaType(r, this)

}