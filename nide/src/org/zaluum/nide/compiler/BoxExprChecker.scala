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

trait BoxExprChecker extends CheckerPart {
  self: OOChecker ⇒
  var scope: ZaluumClassScope = _
  def checkBoxExpr(vs: ValSymbol, c: ClassJavaType) {
    scope = scope(vs)
    var hasApply = false
    lazy val ZComponent = scope.lookupType(Name(classOf[java.awt.Component].getName)).get
    type WithGetAnnotations = { def getAnnotations(): Array[AnnotationBinding] }
      def createPortInstances(ports: Iterable[PortSymbol], vsym: ValSymbol, inside: Boolean, outside: Boolean) = {
        vsym.portInstances :::= (for (p ← ports; if p.isInstanceOf[PortSymbol]) yield {
          val pi = new PortInstance(p.name, p.helperName, vsym, p.dir, Some(p))
          pi
        }).toList;
        vsym.portSides :::= (for (pi ← vsym.portInstances; ps ← pi.portSymbol) yield {
            def define(fromInside: Boolean) = ps.dir match {
              case In    ⇒ List(new PortSide(pi, true, fromInside))
              case Out   ⇒ List(new PortSide(pi, false, fromInside))
              case Shift ⇒ List(new PortSide(pi, true, fromInside), new PortSide(pi, false, fromInside))
            }
          val i = if (inside) define(true) else List()
          val o = if (outside) define(false) else List()
          i ::: o
        }).flatMap(a ⇒ a);
      }
      def visual = c.binding.isCompatibleWith(ZComponent.binding)
      def getAnnotation(o: WithGetAnnotations, c: Class[_]): Option[AnnotationBinding] =
        o.getAnnotations.find { a ⇒
          aToString(a.getAnnotationType.compoundName) == c.getName
        }
      def getApplyAnnotation(o: WithGetAnnotations) =
        getAnnotation(o, classOf[org.zaluum.annotation.Apply])
      def canBeApply(m: MethodBinding) =
        !m.isStatic && !m.isAbstract && m.isPublic
      def processField(f: FieldBinding) {
        val fname = f.name.mkString
        if (getAnnotation(f, classOf[org.zaluum.annotation.Out]).isDefined)
          createPort(Name(fname), f.`type`, Out, field = true)
      }
      def createPort(name: Name, tpe: TypeBinding, dir: PortDir, field: Boolean = false, helperName: Option[Name] = None) {
        val port = new PortSymbol(vs, name, helperName, Point(0, 0), dir, field)
        port.tpe = scope.getJavaType(tpe)
        vs.ports += (port.name -> port)
      }
      def doApply(m: MethodBinding, annotation: AnnotationBinding) {
        hasApply = true
        vs.info = m.selector.mkString
        val argumentNames = BoxExprChecker.annotatedParameters(m, annotation)
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
          createPort(Name(name), p, In, helperName = hName.map { Name(_) })
        }
        m.returnType match {
          case TypeBinding.VOID ⇒ //skip return 
          case r                ⇒ createPort(Name(m.selector.mkString), r, Out)
        }
      }
    val r = c.binding
    val engine = ZaluumCompletionEngineScala.engineFor(scope)
    val allMethods = ZaluumCompletionEngineScala.allMethods(engine, scope, r, static = false)
    val allFields = ZaluumCompletionEngineScala.allFields(engine, scope, r, static = false)
    allMethods
      .map { m ⇒ (m, getApplyAnnotation(m)) }
      .find { case (m, a) ⇒ a.isDefined }
      .foreach { case (m, a) ⇒ doApply(m, a.get) }
    for (f ← allFields; if f.isPublic && !f.isStatic) processField(f)

    vs.isVisual = visual
    val v = vs.decl
    if (!hasApply && !visual) {
      vs.isExecutable = false
      error("Class " + v.typeName.str + " must be visual (extend java.awt.Component) or be marked as @Box and have an @Apply method", v)
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
        error("Cannot find constructor for box " + v.typeName.str +
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
    createPortInstances(vs.ports.values, vs, false, true)
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