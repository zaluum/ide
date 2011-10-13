package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.impl.StringConstant
import org.zaluum.nide.utils.MethodBindingUtils
import org.eclipse.jdt.internal.compiler.lookup.AnnotationBinding
import org.zaluum.nide.utils.JDTUtils.aToString
import org.zaluum.nide.utils.JDTUtils.stringToA
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import scala.collection.mutable.Buffer

trait BoxExprChecker extends CheckerPart {
  self: OOChecker ⇒
  private var scope: ZaluumClassScope = _
  def checkBoxExpr(vs: ValSymbol, c: ClassJavaType) {
    scope = scope(vs)
    val v = vs.decl
    val r = c.binding
    val engine = ZaluumCompletionEngineScala.engineFor(scope)
    var hasApply = false
    lazy val ZComponent = scope.lookupType(Name(classOf[java.awt.Component].getName)).get
    type WithGetAnnotations = { def getAnnotations(): Array[AnnotationBinding] }
      def visual = c.binding.isCompatibleWith(ZComponent.binding)
      def getAnnotation(o: WithGetAnnotations, c: Class[_]): Option[AnnotationBinding] =
        o.getAnnotations.find { a ⇒
          aToString(a.getAnnotationType.compoundName) == c.getName
        }
      def getApplyAnnotation(o: WithGetAnnotations) =
        getAnnotation(o, classOf[org.zaluum.annotation.Apply])
      def canBeApply(m: MethodBinding) =
        !m.isStatic && !m.isAbstract && m.isPublic
      def createPort(name: Name, tpe: TypeBinding, dir: PortDir, field: Boolean, helperName: Option[Name] = None) {
        val port = new PortSymbol(vs, name, None, Point(0, 0), dir, field)
        vs.ports += (name -> port);
        val ps = vs.portSides.find(p ⇒ p.name == name && p.pi.dir == dir) getOrElse {
          if (dir == In)
            vs.createOutsideIn(name, None, Some(port))
          else
            vs.createOutsideOut(name, None, Some(port))
        }
        ps.pi.portSymbol = Some(port)
        ps.pi.tpe = scope.getJavaType(tpe)
        port.tpe = ps.pi.tpe
        ps.pi.missing = false
      }
      def doMethods() {
        // find apply
        val m = vs.getStr(BoxExprType.signatureSymbol) match {
          // look specified method
          case Some(muid) ⇒ Signatures.findMethod(c, scope, muid, false)
          // fall back to @apply
          case None ⇒
            ZaluumCompletionEngineScala.allMethods(engine, scope, r, static = false)
              .find { m ⇒ getApplyAnnotation(m).isDefined }
        }
        m foreach { m ⇒
          hasApply = true
          vs.info = m.selector.mkString
          val argumentNames = getApplyAnnotation(m).flatMap(BoxExprChecker.annotatedParameters(m, _))
          val helpers = BoxExprChecker.helperNames(m, scope)
          val nums = BoxExprChecker.numericNames(m)
          for ((p, i) ← m.parameters zipWithIndex) {
            val (name, hName) = argumentNames match {
              case Some(l) ⇒ (l(i), None)
              case None ⇒ helpers match {
                case Some(h) ⇒ (nums(i), Some(h(i)))
                case None    ⇒ (nums(i), None)
              }
            }
            createPort(Name(name), p, In, false, helperName = hName.map { Name(_) })
          }
          m.returnType match {
            case TypeBinding.VOID ⇒ //skip return 
            case r                ⇒ createPort(Name(m.selector.mkString), r, Out, false)
          }
        }
      }
      def doFields() {
        // FIELDS
        val allFields = ZaluumCompletionEngineScala.allFields(engine, scope, r, static = false)
        val outs = Buffer[FieldBinding]()
        vs.getList(BoxExprType.fieldsDecl) match {
          case Some(fieldNames) ⇒
            for (str ← fieldNames) {
              allFields.find(_.name.mkString == str) match {
                case Some(f) ⇒ outs += f
                case None    ⇒ error("Cannot find output field " + str, v)
              }
            }
            outs ++= allFields.filter(f ⇒ fieldNames.contains(f.name.mkString))
          case None ⇒
        }
        for (
          f ← allFields;
          if (getAnnotation(f, classOf[org.zaluum.annotation.Out]).isDefined) &&
            !outs.contains(f)
        ) outs += f
        for (f ← outs) {
          if (f.isPublic && !f.isStatic)
            createPort(Name(f.name.mkString), f.`type`, Out, true)
          else error("Output field " + f.name.mkString + " must be visible and non static", v)
        }
      }
      def doConfigurer() {
        // configurer
        val boxAnnotation = getAnnotation(r, classOf[org.zaluum.annotation.Box])
        boxAnnotation foreach { a ⇒
          a.getElementValuePairs().find(
            _.getName().mkString == "configurer").map(_.getValue()) match {
              case Some(t: TypeBinding) ⇒ scope.getJavaType(t) match {
                case Some(c: ClassJavaType) ⇒
                  scope.lookupType(Name(classOf[org.zaluum.basic.BoxConfigurer].getName)) foreach { conf ⇒
                    if (c.binding.isCompatibleWith(conf.binding))
                      vs.configurer = Some(c)
                  }
                case _ ⇒
              }
              case _ ⇒
            }
        }
      }
    doMethods()
    doFields()
    doConfigurer()
    vs.isVisual = visual
    if (!hasApply && !vs.isVisual) {
      vs.isExecutable = false
      error("Cannot find method to invoke. Mark with @Apply or modify #Method property", v)
    }
    // Constructor
    val consSignature = vs.getStr(BoxExprType.constructorTypesDecl).getOrElse("()V")
    c.allConstructors.find { _.signature().mkString == consSignature } match {
      case Some(cons) ⇒
        vs.constructor = Some(cons)
        val params = vs.getList(BoxExprType.constructorParamsDecl).getOrElse(List())
        if (params.size != cons.parameters.size)
          error("Number of constructor parameters doesn't match", vs.decl)
        vs.constructorParams = params.zip(cons.parameters) map {
          case (value, binding) ⇒
            val otpe = scope.getJavaType(binding)
            val tpe = otpe.map { _.fqName }.getOrElse(Name("..."))
            val parsed = Values.typeFor(tpe).create(value)
            if (!parsed.valid)
              error("Cannot parse literal \"" + value + "\" to " + tpe.str, vs.decl)
            parsed
        }
      case None ⇒
        error("Cannot find constructor for box " + v.name.str +
          " with signature " + consSignature, v)
    }
    // params
    for (p ← v.params; if !BoxExprType.exprParams.contains(p.key)) {
      c.beanProperties.find(_.name == p.key) match {
        case Some(bean) ⇒
          val parsed = Values.typeFor(bean).create(p.values.mkString)
          if (!parsed.valid)
            error(c.name.str + " cannot parse parameter " + bean.name.str, v)
          vs.setValue(bean, parsed)
        case None ⇒ error(c.name.str + " has no parameter " + p.key.str, v)
      }
    }
    //createPortInstances(vs.ports.values, vs, false, true)
  }
}
object BoxExprChecker {
  def annotatedParameters(m: MethodBinding, annotation: AnnotationBinding) = {
      def arrOption(a: Any) = a match {
        case a: Array[Object] ⇒ Some(a)
        case _                ⇒ None
      }
      def stringConstant(a: Object) = a match {
        case s: StringConstant ⇒ Some(s.stringValue())
      }
    val arrValues = for (
      pair ← annotation.getElementValuePairs.find { _.getName.mkString == "paramNames" };
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
  def numericNames(m: MethodBinding) =
    (1 to m.parameters.length) map { i ⇒ "p" + i } toList
  def helperNames(m: MethodBinding, scope: ZaluumClassScope) =
    MethodBindingUtils.findMethodParameterNamesEnv(m, scope.environment.nameEnvironment).map(_.toList)
}