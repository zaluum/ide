package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ProblemMethodBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.ProblemFieldBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding

class OOChecker(val c: CheckConnections) extends CheckerPart {
  /*
     * helpers for methods and fields
     */
  def processMethod(vs: ValSymbol, m: Option[MethodBinding])(body: MethodBinding ⇒ Unit) = m match {
    case Some(p: ProblemMethodBinding) ⇒
      error("problem method " + p + p.problemId(), vs.decl)
    case Some(m) ⇒
      if (m.returnType != null && m.returnType != TypeBinding.VOID) {
        val out = vs.portInstances find { _.name == Name("return") } getOrElse { vs.createOutsideOut(Name("return")).pi }
        out.missing = false
        out.tpe = cud.zaluumScope.getJavaType(m.returnType)
        if (out.tpe == NoSymbol) error("return type not found", vs.decl)
      }
      for ((p, i) ← m.parameters.zipWithIndex) {
        val name = Name("p" + i)
        val in = vs.portInstances find { _.name == name } getOrElse { vs.createOutsideIn(Name("p" + i)).pi }
        in.missing = false
        in.tpe = cud.zaluumScope.getJavaType(p);
      }
      vs.info = m
      body(m)
    case None ⇒
      error("method not found", vs.decl)
  }
  /*
     *  statics
     */
  def checkStaticExprType(vs: ValSymbol) {
    val tpe = vs.tpe.asInstanceOf[StaticExprType]
    vs.params.get(tpe.classSymbol) match {
      case Some(className: String) ⇒
        cud.zaluumScope.getJavaType(Name(className)) match {
          case Some(c: ClassJavaType) ⇒
            vs.classinfo = c
            tpe match {
              case NewArrayExprType     ⇒ checkNewArray(vs, c)
              case NewExprType          ⇒ checkNew(vs, c)
              case InvokeStaticExprType ⇒ invokeStatic(vs, c)
              case StaticFieldExprType  ⇒ processStaticField(vs, c)
            }
          case Some(p: PrimitiveJavaType) ⇒
            vs.classinfo = p
            tpe match {
              case NewArrayExprType ⇒ checkNewArray(vs, p)
              case _                ⇒ error("Type must be a class", vs.decl)
            }
          case _ ⇒ error("Class " + className + " not found", vs.decl)
        }
      case None ⇒ error("no class specified", vs.decl)
    }
  }
  def checkNewArray(vs: ValSymbol, t: JavaType) {
    val dims = vs.params.get(NewArrayExprType.arrayDimSymbol) match {
      case Some(dimsStr: String) ⇒
        try { dimsStr.toInt } catch { case e ⇒ error("Cannot parse", vs.decl); 1 }
      case None ⇒ 1
    }
    for (i ← 1 to dims) {
      val name = Name("d"+i)
      val in = vs.portInstances find { _.name == name } getOrElse { vs.createOutsideIn(name).pi }
      in.missing = false
      in.tpe = primitives.Int
    }
    val result = NewArrayExprType.thisPort(vs)
    result.tpe = cud.zaluumScope.getArrayType(t,dims)
  }
  def checkNew(vs: ValSymbol, c: ClassJavaType) {
    if (!c.binding.canBeInstantiated()) {
      error("Class " + c.name.str + " cannot be instantiated", vs.decl);
    }
    vs.params.get(NewExprType.signatureSymbol) match {
      case Some(NewExprType.Sig(name, signature)) ⇒
        val cons = ZaluumCompletionEngineScala.findConstructor(cud, scope(vs), c.binding, signature)
        processMethod(vs, cons) { m ⇒
          NewExprType.thisPort(vs).tpe = cud.zaluumScope.getJavaType(m.declaringClass)
        }
      case _ ⇒ error("No constructor specified", vs.decl) // XXdefault?
    }
  }
  def invokeStatic(vs: ValSymbol, c: ClassJavaType) {
    vs.params.get(InvokeStaticExprType.signatureSymbol) match {
      case Some(InvokeStaticExprType.Sig(selector, signature)) ⇒
        val m = ZaluumCompletionEngineScala.findBySignature(cud, scope(vs), c, selector, signature, true)
        processMethod(vs, m) { _ ⇒ }
      case _ ⇒ error("Static method not specified", vs.decl)
    }
  }
  def processStaticField(vs: ValSymbol, c: ClassJavaType) {
    val tpe = vs.tpe.asInstanceOf[SignatureExprType]
    vs.params.get(tpe.signatureSymbol) match {
      case Some(fieldName: String) ⇒
        val f = ZaluumCompletionEngineScala.findField(cud, scope(vs), c.binding, fieldName, true)
        processField(vs, c)
      case _ ⇒ error("Static field not specified", vs.decl)
    }
  }

  /*
     * this
     */
  def checkThisExprType(vs: ValSymbol) {
    val tpe = vs.tpe.asInstanceOf[ThisExprType]
    val thiz = tpe.thisPort(vs)
    val thizOut = tpe.thisOutPort(vs)
    InvokeExprType.signatureSymbol.tpe = cud.zaluumScope.getZJavaLangString // XXX ugly
    connectedFrom(thiz) match {
      case Some((from, blame)) ⇒
        from.tpe match {
          case a: ArrayType ⇒
            thiz.tpe = a
            thizOut.tpe = a
            tpe match {
              case ArrayExprType ⇒ array(vs, a)
              case _             ⇒ error("Type must be a class", vs.decl)
            }
          case c: ClassJavaType ⇒
            thiz.tpe = c
            thizOut.tpe = c
            tpe match {
              case InvokeExprType ⇒ invoke(vs, c)
              case FieldExprType  ⇒ processField(vs, c)
              case ArrayExprType  ⇒ error("Type must be array", vs.decl)
            }
          case _ ⇒
            error("bad type", blame)
        }
      case None ⇒ // not connected
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
      case i if (i > 1) ⇒ cud.zaluumScope.getArrayType(a.of, i - 1)
    }
    aPort.tpe = tpe
    oPort.tpe = tpe
    thisPort
  }
  def processField(vs: ValSymbol, c: ClassJavaType) {
    val tpe = vs.tpe.asInstanceOf[SignatureExprType]
    vs.params.get(tpe.signatureSymbol) match {
      case Some(fieldName: String) ⇒
        ZaluumCompletionEngineScala.findField(cud, scope(vs), c.binding, fieldName, false) match {
          case Some(p: ProblemFieldBinding) ⇒ error("problem field " + p + p.problemId(), vs.decl)
          case Some(f: FieldBinding) ⇒
            val out = vs.tpe.asInstanceOf[ResultExprType].outPort(vs)
            val a = vs.tpe.asInstanceOf[OneParameter].aPort(vs)
            a.tpe = cud.zaluumScope.getJavaType(f.`type`)
            out.tpe = a.tpe
            if (out.tpe == NoSymbol) error("field type not found", vs.decl)
            vs.info = f
          case None ⇒ error("field not found", vs.decl)
        }
      case _ ⇒ error("no field specified", vs.decl)
    }
  }
  def invoke(vs: ValSymbol, c: ClassJavaType) {
    vs.params.get(InvokeExprType.signatureSymbol) match {
      case Some(InvokeExprType.Sig(selector, signature)) ⇒
        val m = ZaluumCompletionEngineScala.findBySignature(cud, scope(vs), c, selector, signature, false)
        processMethod(vs, m) { _ ⇒ }
      case _ ⇒ error("signature missing", vs.decl)
    }
  }
  /*
     * expressions with templates
     */
  def checkTemplateExprType(vs: ValSymbol) = { // FIXME share code with While
    val t = vs.tpe.asInstanceOf[TemplateExprType]
    vs.tdecl.template match {
      case Some(template) ⇒
        if (template.blocks.size != t.requiredBlocks)
          error(t.name.classNameWithoutPackage + " must have " + t.requiredBlocks + " blocks", vs.decl) // FIXME tolerate
        else {
          for (pi ← vs.portInstances; ps ← pi.portSymbol) {
            pi.tpe = ps.tpe
          }
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