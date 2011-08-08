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
  def processField(vs: ValSymbol, f: Option[FieldBinding]) = f match {
    case Some(p: ProblemFieldBinding) ⇒
      error("problem field " + p + p.problemId(), vs.decl)
    case Some(f: FieldBinding) ⇒
      val out = vs.tpe.asInstanceOf[ResultExprType].outPort(vs)
      out.finalTpe = cud.zaluumScope.getJavaType(f.`type`)
      if (out.finalTpe == NoSymbol) error("field type not found", vs.decl)
      vs.info = f
    case None ⇒
      error("field not found", vs.decl)
  }
  def processMethod(vs: ValSymbol, m: Option[MethodBinding])(body: MethodBinding=>Unit) = m match {
    case Some(p: ProblemMethodBinding) ⇒
      error("problem method " + p + p.problemId(), vs.decl)
    case Some(m) ⇒
      if (m.returnType != null && m.returnType != TypeBinding.VOID) {
        val out = vs.portInstances find { _.name == Name("return") } getOrElse { vs.createOutsideOut(Name("return")).pi }
        out.missing = false
        out.finalTpe = cud.zaluumScope.getJavaType(m.returnType)
        if (out.finalTpe == NoSymbol) error("return type not found", vs.decl)
      }
      for ((p, i) ← m.parameters.zipWithIndex) {
        val name = Name("p" + i)
        val in = vs.portInstances find { _.name == name } getOrElse { vs.createOutsideIn(Name("p" + i)).pi }
        in.missing = false
        in.finalTpe = cud.zaluumScope.getJavaType(p);
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
              case NewExprType            ⇒ checkNew(vs, c)
              case InvokeStaticExprType   ⇒ invokeStatic(vs, c)
              case GetStaticFieldExprType ⇒ getStaticField(vs, c)
            }
          case _ ⇒ error("Class " + className + " not found", vs.decl)
        }
      case None ⇒ error("no class specified", vs.decl)
    }
  }
  def checkNew(vs: ValSymbol, c: ClassJavaType) {
    if (!c.binding.canBeInstantiated()) {
      error("Class " + c.name.str + " cannot be instantiated",vs.decl);
    }
    vs.params.get(NewExprType.signatureSymbol) match {
      case Some(NewExprType.Sig(name,signature)) ⇒
        val cons = ZaluumCompletionEngineScala.findConstructor(cud, scope(vs),c.binding, signature)
        processMethod(vs,cons){ m=>
          NewExprType.thisPort(vs).finalTpe = cud.zaluumScope.getJavaType(m.declaringClass)
      	}
      case _ ⇒ error("No constructor specified", vs.decl) // XXdefault?
    }
  }
  def invokeStatic(vs: ValSymbol, c: ClassJavaType) {
    vs.params.get(InvokeStaticExprType.signatureSymbol) match {
      case Some(InvokeStaticExprType.Sig(selector, signature)) ⇒
        val m = ZaluumCompletionEngineScala.findBySignature(cud, scope(vs), c, selector, signature, true)
        processMethod(vs,m){_=>}
      case _ ⇒ error("Static method not specified", vs.decl)
    }
  }
  def getStaticField(vs: ValSymbol, c: ClassJavaType) {
    vs.params.get(GetStaticFieldExprType.signatureSymbol) match {
      case Some(fieldName: String) ⇒
        val f = ZaluumCompletionEngineScala.findField(cud, scope(vs), c.binding, fieldName, true)
        processField(vs, f)
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
        from.finalTpe match {
          case c: ClassJavaType ⇒
            tpe match {
              case InvokeExprType   ⇒ invoke(vs, thiz, thizOut, c)
              case GetFieldExprType ⇒ getField(vs, thiz, thizOut, c)
            }
          case _ ⇒
            error("bad type", blame)
        }
      case None ⇒ // not connected
    }
  }
  def getField(vs: ValSymbol, obj: PortInstance, thisOut: PortInstance, c: ClassJavaType) {
    obj.finalTpe = c
    thisOut.finalTpe = c
    vs.params.get(GetFieldExprType.signatureSymbol) match {
      case Some(fieldName: String) ⇒
        val f = ZaluumCompletionEngineScala.findField(cud, scope(vs), c.binding, fieldName, false)
        processField(vs, f)
      case _ ⇒ error("no field specified", vs.decl)
    }
  }

  def invoke(vs: ValSymbol, obj: PortInstance, thisOut: PortInstance, c: ClassJavaType) {
    obj.finalTpe = c
    thisOut.finalTpe = c
    vs.params.get(InvokeExprType.signatureSymbol) match {
      case Some(InvokeExprType.Sig(selector, signature)) ⇒
        val m = ZaluumCompletionEngineScala.findBySignature(cud, scope(vs), c, selector, signature, false)
        processMethod(vs, m){_=>}
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
            pi.finalTpe = ps.tpe
          }
          t.ports.values foreach { ps ⇒
            val pi = vs.findPortInstance(ps).get
            pi.finalTpe = primitives.Boolean
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