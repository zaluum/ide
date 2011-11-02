package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ProblemMethodBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.ProblemFieldBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.zaluum.nide.utils.MethodBindingUtils

class OOChecker(val c: CheckConnections) extends CheckerPart with BoxExprChecker {
  /*
     * helpers for methods and fields
     */
  def createMethodPorts(vs: ValSymbol, m: Option[MethodBinding]) =
    createMethodPortsAndDo(vs, m) { _ ⇒ }
  def createMethodPortsAndDo(vs: ValSymbol, m: Option[MethodBinding])(body: MethodBinding ⇒ Unit) = m match {
    case Some(p: ProblemMethodBinding) ⇒
      error("Problem with selected method " + p.problemId(), vs.decl) // TODO improve error message
    case Some(m) ⇒
      if (m.returnType != null && m.returnType != TypeBinding.VOID) {
        val out = vs.portInstances find { _.name == Name("return") } getOrElse { vs.createOutsideOut(Name("return")).pi }
        out.missing = false
        out.tpe = ztd.zaluumScope.getJavaType(m.returnType)
        if (out.tpe.isEmpty) error("Return type " + m.returnType.readableName().mkString + " cannot be resolved", vs.decl)
      }
      for ((p, i) ← m.parameters.zipWithIndex) {
        val name = Name("p" + i)
        val in = vs.portInstances find { _.name == name } getOrElse { vs.createOutsideIn(Name("p" + i)).pi }
        in.missing = false
        in.tpe = ztd.zaluumScope.getJavaType(p);
      }
      vs.info = m
      body(m)
    case None ⇒
      error("Method not found", vs.decl)
  }
  /*
     *  statics
     */
  def checkStaticExprType(vs: ValSymbol) {
    val tpe = vs.tpe.get.asInstanceOf[StaticExprType]
    vs.getStr(tpe.typeSymbol) match {
      case Some(str) ⇒
        // XXX possible bug when a name.java and a name.zaluum are created
        if (ztd == null) throw new Exception
        if (ztd.zaluumScope == null) throw new Exception
        ztd.zaluumScope.lookupType(Name(str)) match {
          case Some(cl: ClassJavaType) ⇒
            vs.classinfo = cl
            tpe match {
              case BoxExprType          ⇒ checkBoxExpr(vs, cl); c.assignBoxTypeSymbolTypes(vs)
              case NewArrayExprType     ⇒ checkNewArray(vs, cl)
              case NewExprType          ⇒ checkNew(vs, cl)
              case ArrayComposeExprType ⇒ checkArrayCompose(vs, cl)
              case InvokeStaticExprType ⇒ invokeStatic(vs, cl)
              case StaticFieldExprType  ⇒ processField(vs, cl, true)
            }
          case Some(p: PrimitiveJavaType) ⇒
            vs.classinfo = p
            tpe match {
              case NewArrayExprType     ⇒ checkNewArray(vs, p)
              case ArrayComposeExprType ⇒ checkArrayCompose(vs, p)
              case _                    ⇒ error("Type must be a class", vs.decl)
            }
          case _ ⇒ error("Class " + str + " not found", vs.decl)
        }
      case None ⇒ error("No class specified", vs.decl)
    }
  }
  def checkArrayCompose(vs: ValSymbol, t: JavaType) {
    val size = vs.getStr(ArrayComposeExprType.size) match {
      case Some(s) ⇒ try { s.toInt } catch { case e ⇒ error("Cannot parse size", vs.decl); 1 }
      case None    ⇒ 2
    }
    vs.info = size.asInstanceOf[AnyRef]
    vs.classinfo = t
    for (i ← 0 until size) {
      val name = Name("element" + i)
      val in = vs.portInstances find { _.name == name } getOrElse { vs.createOutsideIn(name).pi }
      in.missing = false
      in.tpe = t
    }
    val out = vs.findPortInstance(ArrayComposeExprType.out).get
    out.missing = false
    if (t != null) {
      out.tpe = ztd.zaluumScope.lookupType(Name(t.fqName.str + "[]"))
    }
  }
  def checkNewArray(vs: ValSymbol, t: JavaType) {
    val dims = vs.getStr(NewArrayExprType.arrayDimSymbol) match {
      case Some(s) ⇒
        try { s.toInt } catch { case e ⇒ error("Cannot parse", vs.decl); 1 }
      case None ⇒ 1
    }
    for (i ← 1 to dims) {
      val name = Name("d" + i)
      val in = vs.portInstances find { _.name == name } getOrElse { vs.createOutsideIn(name).pi }
      in.missing = false
      in.tpe = primitives.Int
    }
    val result = NewArrayExprType.thisPort(vs)
    result.tpe = ztd.zaluumScope.getArrayType(t, dims)
  }
  def checkNew(vs: ValSymbol, c: ClassJavaType) {
    NewExprType.thisPort(vs).tpe = c
    if (!c.binding.canBeInstantiated()) {
      error("Class " + c.name.str + " cannot be instantiated", vs.decl);
    }
    vs.getStr(NewExprType.signatureSymbol) match {
      case Some(muid) ⇒
        val cons = Signatures.findConstructor(c, scope(vs), muid, true)
        createMethodPortsAndDo(vs, cons) { m ⇒
          NewExprType.thisPort(vs).tpe = ztd.zaluumScope.getJavaType(m.declaringClass)
        }
      case _ ⇒ error("No constructor specified", vs.decl) // XXdefault?
    }
  }
  def invokeStatic(vs: ValSymbol, c: ClassJavaType) {
    vs.getStr(InvokeStaticExprType.signatureSymbol) match {
      case Some(muid) ⇒
        val m = Signatures.findMethod(c, scope(vs), muid, true)
        createMethodPorts(vs, m)
      case _ ⇒ error("Static method not specified", vs.decl)
    }
  }
  def checkThisRefExprType(vs: ValSymbol) {
    ThisRefExprType.thisPort(vs).tpe = c.analyzer.toCompile.sym
  }
  /*
     * this
     */
  def checkThisExprType(vs: ValSymbol) {
    val tpe = vs.tpe.get.asInstanceOf[ThisExprType]
    val thiz = tpe.thisPort(vs)
    val thizOut = tpe.thisOutPort(vs)
    // ?? InvokeExprType.signatureSymbol.tpe = ztd.zaluumScope.getZJavaLangString // XXX ugly

    connectedFrom(thiz) match {
      case Some((from, blame)) ⇒
        from.tpe match {
          case Some(a: ArrayType) ⇒
            thiz.tpe = a
            thizOut.tpe = a
            tpe match {
              case ArrayExprType ⇒ array(vs, a)
              case _             ⇒ error("Type must be a class", vs.decl)
            }
          case Some(c: ClassJavaType) ⇒
            thiz.tpe = c
            thizOut.tpe = c
            tpe match {
              case InvokeExprType ⇒ invoke(vs, c)
              case FieldExprType  ⇒ processField(vs, c, false)
              case ArrayExprType  ⇒ error("Type must be array", vs.decl)
            }
          case _ ⇒
            error("Type is not a class", blame)
        }
      case None ⇒ error("Port objectIn must be connected", vs.decl)
    }
  }
  def array(vs: ValSymbol, a: ArrayType) {
    val index = ArrayExprType.indexPort(vs)
    val thisPort = ArrayExprType.thisPort(vs)
    val thisOutPort = ArrayExprType.thisOutPort(vs)
    val aPort = ArrayExprType.aPort(vs)
    val oPort = ArrayExprType.outPort(vs)
    index.tpe = primitives.Int
    val tpe = a.dim match {
      case 1            ⇒ a.of
      case i if (i > 1) ⇒ ztd.zaluumScope.getArrayType(a.of, i - 1)
    }
    aPort.tpe = tpe
    oPort.tpe = tpe
    thisPort
  }
  def processField(vs: ValSymbol, c: ClassJavaType, static: Boolean) {
    val tpe = vs.tpe.get.asInstanceOf[SignatureExprType]
    vs.getStr(tpe.signatureSymbol) match {
      case Some(fieldName) ⇒
        Signatures.findField(c, scope(vs), fieldName, static) match {
          case Some(p: ProblemFieldBinding) ⇒ error("Problem field " + p + p.problemId(), vs.decl)
          case Some(f: FieldBinding) ⇒
            val out = vs.tpe.get.asInstanceOf[ResultExprType].outPort(vs)
            val a = vs.tpe.get.asInstanceOf[OneParameter].aPort(vs)
            a.tpe = ztd.zaluumScope.getJavaType(f.`type`)
            out.tpe = a.tpe
            if (out.tpe.isEmpty) error("Field type not found " + f.`type`.readableName.mkString, vs.decl)
            vs.info = f
          case None ⇒ error("Field not found", vs.decl)
        }
      case _ ⇒ error("No field specified", vs.decl)
    }
  }
  def invoke(vs: ValSymbol, c: ClassJavaType) {
    vs.getStr(InvokeExprType.signatureSymbol) match {
      case Some(muid) ⇒
        val m = Signatures.findMethod(c, scope(vs), muid, false)
        createMethodPorts(vs, m)
      case _ ⇒ error("No selected method specified", vs.decl)
    }
  }
  /*
   * expressions with templates
   */
  def checkTemplateExprType(vs: ValSymbol) = {
    val t = vs.tpe.get.asInstanceOf[TemplateExprType]
    vs.decl.template match {
      case Some(template) ⇒
        if (template.blocks.size != t.requiredBlocks)
          error(t.name.classNameWithoutPackage + " must have " + t.requiredBlocks + " blocks", vs.decl)
        else {
          for (pi ← vs.portInstances; ps ← pi.portSymbol)
            pi.tpe = ps.tpe
          t.ports.values foreach { ps ⇒
            val pi = vs.findPortInstance(ps).get
            pi.tpe = primitives.Boolean
          }
          template.blocks.foreach { b ⇒
            new CheckConnections(b, false, c.analyzer).run()
          }
        }
      case None ⇒
        error("Fatal no template for template expression", vs.decl)
    }
    c.checkPortConnectionsTypes(vs)
  }
}