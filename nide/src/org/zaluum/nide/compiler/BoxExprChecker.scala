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
import BoxExprChecker._
trait BoxExprChecker extends CheckerPart {
  self: OOChecker ⇒

  private def createPort(vs: ValSymbol, name: Name, tpe: TypeBinding, dir: PortDir, field: Boolean, helperName: Option[Name] = None) {
    val port = new PortSymbol(vs, name, helperName, Point(0, 0), dir, field)
    vs.ports += (name -> port);
    val pi = vs.portInstances.find(p ⇒ p.name == name && p.dir == dir) getOrElse {
      if (dir == In)
        vs.createOutsideIn(name, Some(port)).pi
      else
        vs.createOutsideOut(name, Some(port)).pi
    }
    pi.portSymbol = Some(port)
    pi.tpe = scope(vs).getJavaType(tpe)
    port.tpe = pi.tpe
    pi.missing = false
  }

  def selectBestMethodApply(vs: ValSymbol, c: ClassJavaType): Option[MethodBinding] = {
    val withApply = c.allMethods(false, scope(vs)).filter { m ⇒ getApplyAnnotation(m).isDefined }
      def parLen(m: MethodBinding) = if (m.parameters == null) 0 else m.parameters.length
    val grouped = withApply.groupBy(parLen)
    for (num ← grouped.keys.toSeq.sorted) {
      val incoming = vs.incomingTypes(num)
      val matching = grouped(num).filter(m ⇒ c.matchesParameters(m, incoming, scope(vs)))
      if (matching.size == 1) return Some(matching.head)
      else if (matching.size > 1) {
        val tie = matching.map(m ⇒ (m, numberParametersMatching(m, incoming))).sortBy(_._2)
        if (tie(0)._2 == tie(1)._2)
          error("Ambigous method with apply...", vs.decl)
        return Some(tie(0)._1)
      }
      matching
    }
    None
  }
  private def doMethods(vs: ValSymbol, c: ClassJavaType) {
    // find apply
    val r = c.binding
    val om = vs.getStr(vs.tpe.get.asInstanceOf[SignatureExprType].signatureSymbol) match {
      case Some(Signatures.MethodAndArity(selector, arity)) ⇒
        selectBestMethod(vs, c, false, false)
      case _ ⇒
        selectBestMethodApply(vs, c)
    }

    om foreach { m ⇒
      vs.typeSpecificInfo = Some(m)
      for ((p, name, hName) ← BoxExprChecker.parameterNames(m, scope(vs))) {
        createPort(vs, name, p, In, false, helperName = hName)
      }
      m.returnType match {
        case TypeBinding.VOID ⇒ //skip return 
        case r                ⇒ createPort(vs, Name(m.selector.mkString), r, Out, false)
      }
    }
  }
  private def doFields(vs: ValSymbol, c: ClassJavaType) {
    // FIELDS
    val fields = c.allFields(false, scope(vs))
    val outs = Buffer[FieldBinding]()
    vs.getList(BoxExprType.fieldsDecl) match {
      case Some(fieldNames) ⇒
        for (str ← fieldNames) {
          fields.find(_.name.mkString == str) match {
            case Some(f) ⇒ outs += f
            case None    ⇒ error("Cannot find output field " + str, vs.decl)
          }
        }
        outs ++= fields.filter(f ⇒ fieldNames.contains(f.name.mkString))
      case None ⇒
    }
    for (
      f ← fields;
      if (getAnnotation(f, classOf[org.zaluum.annotation.Out]).isDefined) &&
        !outs.contains(f)
    ) outs += f
    for (f ← outs) {
      if (f.isPublic && !f.isStatic)
        createPort(vs, Name(f.name.mkString), f.`type`, Out, true)
      else error("Output field " + f.name.mkString + " must be visible and non static", vs.decl)
    }
  }
  def doConfigurer(vs: ValSymbol, r: ReferenceBinding) {
    // configurer
    val boxAnnotation = getAnnotation(r, classOf[org.zaluum.annotation.Box])
    boxAnnotation foreach { a ⇒
      a.getElementValuePairs().find(
        _.getName().mkString == "configurer").map(_.getValue()) match {
          case Some(t: TypeBinding) ⇒ scope(vs).getJavaType(t) match {
            case Some(c: ClassJavaType) ⇒
              scope(vs).lookupType(Name(classOf[org.zaluum.basic.BoxConfigurer].getName)) foreach { conf ⇒
                if (c.binding.isCompatibleWith(conf.binding))
                  vs.configurer = Some(c)
              }
            case _ ⇒
          }
          case _ ⇒
        }
    }
  }
  private def doConstructor(vs: ValSymbol, c: ClassJavaType) {
    // Constructor
    val consSignature = vs.getStr(BoxExprType.constructorTypesDecl).getOrElse("()V")
    c.allConstructors(scope(vs)).find { _.signature().mkString == consSignature } match {
      case Some(cons) ⇒
        vs.constructor = Some(cons)
        val params = vs.getList(BoxExprType.constructorParamsDecl).getOrElse(List())
        if (params.size != cons.parameters.size)
          error("Number of constructor parameters doesn't match", vs.decl)
        vs.constructorParams = params.zip(cons.parameters) map {
          case (value, binding) ⇒
            val otpe = scope(vs).getJavaType(binding)
            val tpe = otpe.map { _.fqName }.getOrElse(Name("..."))
            val parsed = Values.typeFor(tpe).create(value)
            if (!parsed.valid)
              error("Cannot parse literal \"" + value + "\" to " + tpe.str, vs.decl)
            parsed
        }
      case None ⇒
        error("Cannot find constructor for box " + vs.decl.name.str +
          " with signature " + consSignature, vs.decl)
    }
  }
  private def doParams(vs: ValSymbol, c: ClassJavaType) {
    // params
    for (p ← vs.decl.params; if !BoxExprType.exprParams.contains(p.key)) {
      c.beanProperties.find(_.name == p.key) match {
        case Some(bean) ⇒
          val parsed = Values.typeFor(bean).create(p.values.mkString)
          if (!parsed.valid)
            error(c.name.str + " cannot parse parameter " + bean.name.str, vs.decl)
          vs.setValue(bean, parsed)
        case None ⇒ error(c.name.str + " has no parameter " + p.key.str, vs.decl)
      }
    }
  }

  def checkBoxExpr(vs: ValSymbol, c: ClassJavaType) {
    val v = vs.decl
    val r = c.binding
    lazy val ZComponent = scope(vs).lookupType(Name(classOf[java.awt.Component].getName)).get
    val visual = c.binding.isCompatibleWith(ZComponent.binding)
      def canBeApply(m: MethodBinding) =
        !m.isStatic && !m.isAbstract && m.isPublic

    doMethods(vs, c)
    doFields(vs, c)
    doConfigurer(vs, r)
    vs.isVisual = visual
    if (vs.typeSpecificInfo.isEmpty) {
      vs.isExecutable = false
      error("Cannot find method to invoke. Mark with @Apply or modify #Method property", v)
    }
    doConstructor(vs, c)
    doParams(vs, c)
    for (pi ← vs.portInstances; ps ← pi.portSymbol) {
      assert(pi.tpe == ps.tpe && pi.missing == false)
    }
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
  def numericNames(m: MethodBinding): List[String] =
    (1 to m.parameters.length) map { i ⇒ "p" + i } toList

  def helperNames(m: MethodBinding, scope: ZaluumClassScope): Option[List[String]] =
    MethodBindingUtils.findMethodParameterNamesEnv(m, scope.environment.nameEnvironment).map(_.toList)

  def parameterNames(m: MethodBinding, scope: ZaluumClassScope) = {
    val argumentNames = getApplyAnnotation(m).flatMap(BoxExprChecker.annotatedParameters(m, _))
    val helpers = BoxExprChecker.helperNames(m, scope)
    val nums = BoxExprChecker.numericNames(m)
    for ((p, i) ← m.parameters zipWithIndex) yield {
      val (name, hName) = argumentNames match {
        case Some(l) if l.length > i ⇒ (l(i), None)
        case _ ⇒ helpers match {
          case Some(h) ⇒ (nums(i), Some(h(i)))
          case None    ⇒ (nums(i), None)
        }
      }
      (p, Name(name), hName map { Name(_) })
    }
  }

  private type WithGetAnnotations = { def getAnnotations(): Array[AnnotationBinding] }
  private def getApplyAnnotation(o: WithGetAnnotations) =
    getAnnotation(o, classOf[org.zaluum.annotation.Apply])
  private def getAnnotation(o: WithGetAnnotations, c: Class[_]): Option[AnnotationBinding] =
    o.getAnnotations.find { a ⇒
      aToString(a.getAnnotationType.compoundName) == c.getName
    }

}