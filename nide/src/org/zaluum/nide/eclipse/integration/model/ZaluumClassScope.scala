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
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.ClassJavaType
import org.zaluum.nide.compiler.SimpleClassJavaType
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
  def getBoxedType(p: PrimitiveJavaType): JavaType =
    getJavaType(p.boxedName)
  lazy val ZComponent = getJavaType(Name(classOf[java.awt.Component].getName))
  def getZJavaLangString = getJavaType(Name("java.lang.String"))
  def javaScope: ZaluumClassScope = this
  def lookupType(name: Name): Option[JavaType] = getJavaType(name) match {
    case NoSymbol ⇒ None
    case o        ⇒ Some(o)
  }
  def getArrayType(t: JavaType, dim: Int): ArrayType = {
    val bind = createArrayType(t.binding, dim)
    val a = new ArrayType(this, t, dim, bind)
    a
  }
  def getJavaType(name: Name): JavaType = {
    val arr = name.asArray
    if (arr.isDefined) {
      val (leafname, dim) = arr.get
      val t = getJavaType(leafname)
      if (t == NoSymbol) t
      else getArrayType(t, dim)
    } else {
      val tpe =
        if (name.str.contains(".")) {
          val compoundName = stringToA(name.str)
          getType(compoundName, compoundName.length)
        } else {
          getType(name.str.toCharArray)
        }
      getJavaType(tpe)
    }
  }
  def getStaticMethod(nameHashAndSignature: String): Option[MethodBinding] = {
    MethodBindingUtils.staticMethod(nameHashAndSignature) match {
      case Some((cl, selector, params, ret)) ⇒
        val compound = JDTUtils.stringToA(cl)
        val t = getType(compound, compound.length);

        val paramsBind: List[TypeBinding] = params.map { s ⇒ getJavaType(Name(s)).binding }
        if (paramsBind.contains(null))
          None
        else {
          Some(getMethod(t, selector.toCharArray(), paramsBind.toArray, new FakeInvocationSite(null)))
        }
      case _ ⇒ None
    }
  }
  def getJavaType(tpe: TypeBinding): JavaType = {
    tpe match {
      case m: MissingTypeBinding         ⇒ NoSymbol
      case p: ProblemReferenceBinding    ⇒ NoSymbol
      case u: UnresolvedReferenceBinding ⇒ NoSymbol
      case r: ReferenceBinding ⇒
        val tpe = environment.convertToRawType(r, false).asInstanceOf[ReferenceBinding]
        if (tpe != null) {
          cacheJava.getOrElseUpdate(tpe, create(tpe))
        } else NoSymbol
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
        }
      case a: ArrayBinding ⇒
        val leaf = getJavaType(a.leafComponentType)
        val t = new ArrayType(this, leaf, a.dimensions, a)
        t
    }
  }
  def lookupBoxType(name: Name): Option[BoxTypeSymbol] =
    getJavaType(name) match {
      case bs: BoxTypeSymbol ⇒ Some(bs)
      case _                 ⇒ None
    }

  protected def create(r: ReferenceBinding) = createBoxType(r).getOrElse {
    new SimpleClassJavaType(this, r, this)
  }

  protected def createBoxType(r: ReferenceBinding): Option[BoxTypeSymbol] = {
    val annotation = r.getAnnotations.find { a ⇒
      aToString(a.getAnnotationType.compoundName) == classOf[org.zaluum.annotation.Box].getName
    }
    annotation map { _ ⇒
      val srcName = Name(r.compoundName.last.mkString)
      val pkgName = Name(r.qualifiedPackageName.mkString)
      val visual = r.isCompatibleWith(ZComponent.binding)
      val bs = new BoxTypeSymbol(
        None, visual, r, this)

      val engine = ZaluumCompletionEngineScala.engineFor(this)
      val allMethods = ZaluumCompletionEngineScala.allMethods(engine, this, r, static = false)
      val allFields = ZaluumCompletionEngineScala.allFields(engine, this, r, static = false)
      val allConstructors = ZaluumCompletionEngineScala.allConstructors(engine, this, r)
      for (m ← allConstructors) processMethod(bs, m)
      for (m ← allMethods) processMethod(bs, m)
      for (f ← allFields; if f.isPublic && !f.isStatic) processField(bs, f)
      if (bs.constructors.isEmpty)
        bs.constructors = List(new Constructor(bs, List()))
      bs
    }
  }
  private def processMethod(bs: BoxTypeSymbol, m: MethodBinding) {
    val annotation = m.getAnnotations.find { a ⇒
      aToString(a.getAnnotationType.compoundName) == classOf[org.zaluum.annotation.Apply].getName
    }
    val mName = m.selector.mkString
    if (m.isConstructor && m.isPublic)
      doConstructor(bs, m)
    else if (annotation.isDefined && !m.isStatic && !m.isAbstract && m.isPublic && !bs.hasApply)
      doApply(bs, m, annotation)
  }
  private def processField(bs: BoxTypeSymbol, f: FieldBinding) {
    val fname = f.name.mkString
      def hasAnnotation(c: Class[_]) = f.getAnnotations.exists { a ⇒
        aToString(a.getAnnotationType.compoundName) == c.getName
      }
    if (hasAnnotation(classOf[org.zaluum.annotation.Out]))
      createPort(bs, Name(fname), f.`type`, Out, field = true)
  }
  private def createPort(bs: BoxTypeSymbol, name: Name, tpe: TypeBinding, dir: PortDir, field: Boolean = false, helperName: Option[Name] = None) {
    val port = new PortSymbol(bs, name, helperName, Point(0, 0), dir, field)
    port.tpe = getJavaType(tpe)
    bs.ports += (port.name -> port)
  }
  private def doConstructor(bs: BoxTypeSymbol, m: MethodBinding) {
    val names = numericNames(m)
    val params = for ((p, i) ← m.parameters zipWithIndex) yield {
      val ps = new ParamSymbol(bs, Name(names(i))) // helper name
      ps.tpe = getJavaType(p)
      ps
    }
    bs.constructors = new Constructor(bs, params.toList) :: bs.constructors
  }
  private def doApply(bs: BoxTypeSymbol, m: MethodBinding, annotation: Option[AnnotationBinding]) {
    val argumentNames = annotatedParameters(bs, m, annotation)
    val helpers = helperNames(m)
    val nums = numericNames(m)
    for ((p, i) ← m.parameters zipWithIndex) {
      val (name, hName) = argumentNames match {
        case Some(l) ⇒ (l(i), None)
        case None ⇒ helpers match {
          case Some(h) ⇒ (nums(i), Some(h(i)))
          case None    ⇒ (nums(i), None)
        }
      }
      createPort(bs, Name(name), p, In, helperName = hName.map { Name(_) })
    }
    bs.methodSelector = Name(m.selector.mkString)
    m.returnType match {
      case TypeBinding.VOID ⇒ //skip return 
      case r                ⇒ createPort(bs, bs.methodSelector, r, Out)
    }
  }
  private def annotatedParameters(bs: BoxTypeSymbol, m: MethodBinding, annotation: Option[AnnotationBinding]) = {
      def arrOption(a: Any) = a match {
        case a: Array[Object] ⇒ Some(a)
        case _                ⇒ None
      }
      def stringConstant(a: Object) = a match {
        case s: StringConstant ⇒ Some(s.stringValue())
      }
    bs.hasApply = true
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
  private def numericNames(m: MethodBinding) =
    (1 to m.parameters.length) map { i ⇒ "p" + i } toList
  private def helperNames(m: MethodBinding) =
    MethodBindingUtils.findMethodParameterNamesEnv(m, environment.nameEnvironment).map(_.toList)

}