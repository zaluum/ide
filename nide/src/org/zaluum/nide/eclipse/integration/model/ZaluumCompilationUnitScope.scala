package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.Scope
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.compiler._
import org.eclipse.jdt.internal.compiler.lookup.BinaryTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding
import org.zaluum.annotation.Box
import JDTInternalUtils._
import scala.collection.mutable.Map
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.env.ISourceType
import org.eclipse.jdt.core.Signature
import org.eclipse.jdt.core.JavaModelException
import org.eclipse.jdt.internal.core.SourceMethod
import org.eclipse.jdt.internal.core.SourceMethodElementInfo
import org.eclipse.jdt.internal.core.SourceTypeElementInfo
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
class ZaluumCompilationUnitScope(cud: ZaluumCompilationUnitDeclaration, lookupEnvironment: LookupEnvironment) extends CompilationUnitScope(cud, lookupEnvironment) {
  override protected def buildClassScope(parent: Scope, typeDecl: TypeDeclaration) = {
    new ZaluumClassScope(parent, typeDecl)
  }
  def getExpectedPackageName = this.referenceContext.compilationResult.compilationUnit.getPackageName();
  val cache = Map[Name, BoxTypeSymbol]()
  val cacheJava = Map[TypeBinding, ClassJavaType]()
  def getJavaType(name: Name): Option[JavaType] = {
    val arr = name.asArray
    if (arr.isDefined) {
      val (leafname, dim) = arr.get
      getJavaType(leafname) map { l ⇒
        new ArrayType(cud.JDTScope, l, dim)
      }
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
  def getJavaType(tpe: TypeBinding): Option[JavaType] = {
    tpe match {
      case r: ReferenceBinding ⇒
        cacheJava.get(tpe).orElse {
          val jtpe = new ClassJavaType(cud.JDTScope, Name(aToString(r.compoundName)))
          cacheJava += (tpe -> jtpe)
          Some(jtpe)
        }
      case b: BaseTypeBinding ⇒
        b.simpleName.mkString match {
          case "byte" ⇒ Some(primitives.Byte)
          case "short" ⇒ Some(primitives.Short)
          case "int" ⇒ Some(primitives.Int)
          case "long" ⇒ Some(primitives.Long)
          case "float" ⇒ Some(primitives.Float)
          case "double" ⇒ Some(primitives.Double)
          case "boolean" ⇒ Some(primitives.Boolean)
          case "char" ⇒ Some(primitives.Char)
          case _ ⇒ None
        }
      case a: ArrayBinding ⇒
        getJavaType(a.leafComponentType) map { leaf ⇒
          new ArrayType(cud.JDTScope, leaf, a.dimensions)
        }
    }
  }
  def findMethodParameterNamesSource(m: MethodBinding, sourceType: SourceTypeBinding): Option[Array[String]] = {
    if (sourceType.scope != null) {
      val parsedType = sourceType.scope.referenceContext
      if (parsedType != null) {
        val methodDecl = parsedType.declarationOf(m.original());
        if (methodDecl != null) {
          val arguments = methodDecl.arguments;
          if (arguments != null) {
            val names = for (a ← arguments) yield { a.name.mkString }
            return Some(names)
          }
        }
      }
    }
    None
  }
  private def findMethodParameterNamesBinary(m: MethodBinding, rb: ReferenceBinding): Option[Array[String]] = {
    environment.nameEnvironment.findType(rb.compoundName) match {
      case null ⇒ None
      case answer if answer.isSourceType && answer.getSourceTypes()(0) != null ⇒
        val sourceType = answer.getSourceTypes()(0);
        val typeHandle = sourceType.asInstanceOf[SourceTypeElementInfo].getHandle();
        val signature = for (e ← m.parameters) yield {
          e.signature.mkString
        }
        val searchedMethod = typeHandle.getMethod(String.valueOf(m.selector), signature);
        val foundMethods = typeHandle.findMethods(searchedMethod);
        if (foundMethods != null && foundMethods.length == 1) {
          try {
            val names = foundMethods(0).asInstanceOf[SourceMethod]
              .getElementInfo.asInstanceOf[SourceMethodElementInfo]
              .getArgumentNames().map { _.mkString }
            Some(names)
          } catch { case e: JavaModelException ⇒ None }
        } else None
      case answer if answer.isBinaryType ⇒
        answer.getBinaryType.getMethods.find { candidate ⇒
          candidate.getSelector.mkString == m.selector.mkString &&
            candidate.getMethodDescriptor.mkString == m.signature.mkString
        } map { foundM ⇒ foundM.getArgumentNames map { _.mkString } }
    }
  }
  private def findMethodParameterNames(m: MethodBinding): Option[Array[String]] = {
    val erasure = m.declaringClass.erasure();
    erasure match {
      case sourceType: SourceTypeBinding ⇒
        findMethodParameterNamesSource(m, sourceType)
      case rb: ReferenceBinding ⇒
        findMethodParameterNamesBinary(m, rb)
      case _ ⇒ None
    }
  }
  def allFieldsFor(r:ReferenceBinding) : List[FieldBinding]= {
    r.fields.toList ++ {if (r.superclass!=null) allFieldsFor(r.superclass) else List()}
  }
  def allMethodsFor(r:ReferenceBinding) : List[MethodBinding]= {
    r.methods.toList ++ {if (r.superclass!=null) allMethodsFor(r.superclass) else List()}
  }
  def getBoxType(name: Name): Option[BoxTypeSymbol] = {
    cache.get(name).orElse {
      val compoundName = stringToA(name.str)
      getType(compoundName, compoundName.length) match {
        case r: ReferenceBinding ⇒
          // should not check for Box annotation to let boxes inherit non annotated classes
          val sperO = r.superclass match {
            case null ⇒ None
            case spr ⇒ aToString(spr.compoundName) match {
              case null ⇒ None
              case "java.lang.Object" ⇒ None
              case other ⇒ Some(Name(other))
            }
          }
          
          val srcName = Name(r.compoundName.last.mkString)
          val pkgName = Name(r.qualifiedPackageName.mkString) 
          val bs = new BoxTypeSymbol(
            cud.a.global.root, srcName, pkgName,
            sperO, None, None, r.isAbstract)
          bs.scope = cud.a.global
          for (f ← allFieldsFor(r); if f.isPublic && !f.isStatic) {
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
          
          for (m ← allMethodsFor(r)) {
            val parameterNames = findMethodParameterNames(m) getOrElse {
              (for (i ← 0 until m.parameters.length) yield "$" + i).toArray
            }
            val mName = m.selector.mkString
            if (m.isConstructor && m.isPublic) {
              val params = for ((p, name) ← m.parameters zip parameterNames) yield {
                val ps = new ParamSymbol(bs, Name(name))
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
              }else if (mName == "apply" && !m.isStatic && !m.isAbstract && m.parameters.size==0 && m.returnType == TypeBinding.VOID) {
                bs.hasApply= true
              }
            }
          }
          if (bs.constructors.isEmpty)
            bs.constructors = List(new Constructor(bs, List()))
          cache += (name -> bs)
          Some(bs)
        case a ⇒ None
      }
    }
  }
}