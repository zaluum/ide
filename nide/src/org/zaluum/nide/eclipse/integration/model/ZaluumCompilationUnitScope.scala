package org.zaluum.nide.eclipse.integration.model

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
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.ClassJavaType
import org.zaluum.nide.compiler.Constructor
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.JavaType
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.NoSymbol
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.ParamSymbol
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.PortSymbol
import org.zaluum.nide.compiler.PrimitiveJavaType
import org.zaluum.nide.compiler.{ Scope ⇒ ZScope }
import org.zaluum.nide.compiler.Type
import org.zaluum.nide.compiler.primitives
import JDTInternalUtils.aToString
import JDTInternalUtils.stringToA
import org.zaluum.nide.compiler.ZaluumCompletionEngineScala
import org.eclipse.jdt.internal.compiler.impl.StringConstant
import org.zaluum.nide.compiler.PortDir
class ZaluumCompilationUnitScope(cudp: ZaluumCompilationUnitDeclaration, lookupEnvironment: LookupEnvironment) extends CompilationUnitScope(cudp, lookupEnvironment) with ZScope {
  override protected def buildClassScope(parent: Scope, typeDecl: TypeDeclaration) = {
    new ZaluumClassScope(parent, typeDecl)
  }
  def getExpectedPackageName = this.referenceContext.compilationResult.compilationUnit.getPackageName();
  val cache = Map[Name, BoxTypeSymbol]()
  private val cacheJava = Map[TypeBinding, ClassJavaType]()

  def cud = referenceContext.asInstanceOf[ZaluumCompilationUnitDeclaration]
  def name = Name("root")
  def owner = null
  def getBoxedType(p: PrimitiveJavaType): JavaType =
    getJavaType(p.boxedName).get

  def getZJavaLangString = getJavaType(Name("java.lang.String")).get;
  def javaScope: ZaluumCompilationUnitScope = this
  def lookupType(name: Name): Option[Type] = getJavaType(name)
  def getArrayType(t: JavaType, dim: Int): ArrayType = {
    val bind = createArrayType(t.binding, dim)
    val a = new ArrayType(this, t, dim)
    a.binding = bind
    a
  }
  def getJavaType(name: Name): Option[JavaType] = {
    val arr = name.asArray
    if (arr.isDefined) {
      val (leafname, dim) = arr.get
      getJavaType(leafname) map { t ⇒
        getArrayType(t, dim)
      }
    } else {
      val tpe =
        if (name.str.contains(".")) {
          val compoundName = stringToA(name.str)
          getType(compoundName, compoundName.length)
        } else {
          getType(name.str.toCharArray)
        }
      Some(getJavaType(tpe)) //FIXME
    }
  }
  def getJavaType(tpe: TypeBinding): JavaType = {
    tpe match {
      case m: MissingTypeBinding         ⇒ NoSymbol
      case p: ProblemReferenceBinding    ⇒ NoSymbol
      case u: UnresolvedReferenceBinding ⇒ NoSymbol
      case r: ReferenceBinding ⇒
        val tpe = lookupEnvironment.convertToRawType(r, false).asInstanceOf[ReferenceBinding]
        cacheJava.getOrElseUpdate(tpe, {
          val jtpe = new ClassJavaType(this, Name(aToString(tpe.compoundName)))
          jtpe.binding = tpe
          jtpe
        })
      case b: BaseTypeBinding ⇒
        b.simpleName.mkString match {
          case "byte"    ⇒ primitives.Byte
          case "short"   ⇒ primitives.Short
          case "int"     ⇒ primitives.Int
          case "long"    ⇒ primitives.Long
          case "float"   ⇒ primitives.Float
          case "double"  ⇒ primitives.Double
          case "boolean" ⇒ primitives.Boolean
          case "char"    ⇒ primitives.Char
          //case _ ⇒ None
        }
      case a: ArrayBinding ⇒
        val leaf = getJavaType(a.leafComponentType)
        val t = new ArrayType(this, leaf, a.dimensions)
        t.binding = a
        t
    }
  }

  def allFieldsFor(r: ReferenceBinding): List[FieldBinding] = {
    r.fields.toList ++ { if (r.superclass != null) allFieldsFor(r.superclass) else List() }
  }
  def allMethodsFor(r: ReferenceBinding): List[MethodBinding] = {
    r.methods.toList ++ { if (r.superclass != null) allMethodsFor(r.superclass) else List() }
  }

  def lookupBoxType(name: Name): Option[BoxTypeSymbol] = {
    cache.get(name).orElse {
      val compoundName = stringToA(name.str)
      getType(compoundName, compoundName.length) match {
        case r: ReferenceBinding ⇒
          val srcName = Name(r.compoundName.last.mkString)
          val pkgName = Name(r.qualifiedPackageName.mkString)
          val bs = new BoxTypeSymbol(
            srcName, pkgName,
            None, None, r.isAbstract)
          bs.scope = this
            def createPort(name: Name, tpe: TypeBinding, dir: PortDir, field:Boolean=false) {
              val port = new PortSymbol(bs, name, name, Point(0, 0), dir,field)
              port.tpe = getJavaType(tpe)
              bs.ports += (port.name -> port)
            }
          /*
             TODO use this. We need a ClassScope, so move this to ZaluumClassScope
            val engine = new ZaluumCompletionEngine(environment)
            ZaluumCompletionEngineScala.allMethods(engine, null, r, false)*/
          for (m ← allMethodsFor(r)) {
            val annotation = m.getAnnotations.find { a ⇒
              aToString(a.getAnnotationType.compoundName) == classOf[org.zaluum.annotation.Apply].getName
            }
            val hasAnnotation = annotation.isDefined

            val helperNames = MethodUtils.findMethodParameterNamesEnv(m, environment.nameEnvironment)
            val numericNames = (1 to m.parameters.length) map { i ⇒ "p" + i } toList
            
            val mName = m.selector.mkString
            if (m.isConstructor && m.isPublic) {
              val params = for ((p, i) ← m.parameters zipWithIndex) yield {
                val name = numericNames(i)
                val ps = new ParamSymbol(bs, Name(name.toString)) // helper name
                ps.tpe = getJavaType(p)
                ps
              }
              bs.constructors = new Constructor(bs, params.toList) :: bs.constructors
            } else {
              if (mName.startsWith("set") && m.parameters.size == 1 && m.returnType == TypeBinding.VOID) {
                val ptpe = getJavaType(m.parameters.head)
                val p = new ParamSymbol(bs, Name(mName))
                p.tpe = ptpe
                bs.params += (p.name -> p)
              } else if (hasAnnotation &&
                !m.isStatic && !m.isAbstract && m.isPublic && !bs.hasApply) {
                  def arrOption(a: Any) = a match {
                    case a: Array[Object] ⇒ Some(a)
                    case _                ⇒ None
                  }
                  def stringConstant(a: Object) = a match {
                    case s: StringConstant ⇒ Some(s.stringValue())
                  }
                val annotatedParameters = {
                  val arrValues = for (
                    a ← annotation;
                    pair ← a.getElementValuePairs.find { _.getName.mkString == "paramNames" };
                    arr ← arrOption(pair.getValue)
                  ) yield { arr }
                  val names = arrValues.map { arr ⇒
                    for (
                      component ← arr.toList;
                      str ← stringConstant(component)
                    ) yield str
                  }
                  names match {
                    case Some(l) if l.size == m.parameters.size ⇒ Some(l)
                    case _                                      ⇒ None
                  }
                }
                val argumentNames = annotatedParameters.getOrElse(numericNames)
                bs.hasApply = true
                for ((p, name) ← m.parameters zip argumentNames) {
                  createPort(Name(name), p, In)
                }
                m.returnType match {
                  case TypeBinding.VOID ⇒ //skip return 
                  case r                ⇒ createPort(Name(m.selector.mkString), r,Out)
                }
              }
            }
          }
          for (f ← allFieldsFor(r); if f.isPublic && !f.isStatic) {
            val fname = f.name.mkString
              def hasAnnotation(c: Class[_]) = f.getAnnotations.exists { a ⇒
                aToString(a.getAnnotationType.compoundName) == c.getName
              }
            if (hasAnnotation(classOf[org.zaluum.annotation.Out]))
              createPort(Name(fname), f.`type`,Out,field=true)
            if (fname == "_widget") {
              f.`type` match {
                case r: ReferenceBinding ⇒
                  bs.visualClass = Some(Name(aToString(r.compoundName)))
                case _ ⇒
              }
            }
          }

          if (bs.constructors.isEmpty)
            bs.constructors = List(new Constructor(bs, List()))
          bs.binding = r
          cache += (name -> bs)
          Some(bs)
        case a ⇒ None
      }
    }
  }
}