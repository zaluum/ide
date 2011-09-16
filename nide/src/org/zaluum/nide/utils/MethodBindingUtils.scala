package org.zaluum.nide.utils
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.JavaModelException
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.env.INameEnvironment
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.eclipse.jdt.internal.compiler.lookup.ExtraCompilerModifiers
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding
import org.eclipse.jdt.internal.core.SourceMethod
import org.eclipse.jdt.internal.core.SourceMethodElementInfo
import org.eclipse.jdt.internal.core.SourceTypeElementInfo
import org.objectweb.asm.util.ASMifiable
import org.objectweb.asm.signature.SignatureReader
import org.objectweb.asm.signature.SignatureVisitor
import scala.collection.mutable.Buffer
import org.zaluum.nide.compiler.primitives

object MethodBindingUtils {
  val StaticRegexp = """(.*)#(.*)(\(.*)""".r
  def staticMethodSplit(str: String): Option[(String, String)] = {
    val spl = str.split("#")
    if (spl.size == 2) Some((spl(0), spl(1)))
    else None
  }
  def staticMethod(str: String): Option[(String, String, List[String], String)] = {
    str match {
      case StaticRegexp(cl, selector, params) ⇒
        try {
          val (tparams, ret) = signatureToTypes(params)
          Some((cl, selector, tparams, ret))
        } catch { case e ⇒ None }
      case _ ⇒ None
    }
  }
  def toMethodSig(m: MethodBinding) = m.selector.mkString + m.signature().mkString
  def signatureToTypes(str: String): (List[String], String) = {
    var ret = ""
    val params = Buffer[String]()
    var dim = 0
    var param = false
      def add(str: String) {
        val name = str + ("[]" * dim)
        if (param) params += name else ret = name
      }
    val s = new SignatureReader(str).accept(new SignatureVisitor() {
      def visitFormalTypeParameter(name: String) {}
      def visitClassBound(): SignatureVisitor = null
      def visitInterfaceBound(): SignatureVisitor = null
      def visitSuperclass(): SignatureVisitor = null
      def visitInterface(): SignatureVisitor = null
      def visitParameterType(): SignatureVisitor = { dim = 0; param = true; this }
      def visitReturnType(): SignatureVisitor = { param = false; dim = 0; this }
      def visitExceptionType(): SignatureVisitor = null
      def visitBaseType(descriptor: Char) {
        add(if (descriptor == 'V') "void" else primitives.fromChar(descriptor).name.str)
      }
      def visitTypeVariable(name: String) {}
      def visitArrayType(): SignatureVisitor = { dim = dim + 1; this }
      def visitClassType(name: String) {
        add(name.replace('/', '.'))
      }
      def visitInnerClassType(name: String) {}
      def visitTypeArgument() {}
      def visitTypeArgument(wildcard: Char) = null
      def visitEnd() {}
    });
    (params.toList, ret)
  }

  def toMethodStr(m: MethodBinding, paramNames: List[String]) = {
    val output = new StringBuffer(10);
    if ((m.modifiers & ExtraCompilerModifiers.AccUnresolved) != 0) {
      output.append("[unresolved] "); //$NON-NLS-1$
    }
    ASTNode.printModifiers(m.modifiers, output);
    output.append(if (m.returnType != null) m.returnType.debugName() else "<no type>");
    output.append(" "); //$NON-NLS-1$
    output.append(if (m.selector != null) new String(m.selector) else "<no selector>");
    output.append("("); //$NON-NLS-1$
    if (m.parameters != null) {
      if (m.parameters != Binding.NO_PARAMETERS) {
        val padded = paramNames.padTo(m.parameters.length, "?")
        val zip = padded.zip(m.parameters)
        val str = zip.map {
          case (name, p) ⇒
            if (p != null) p.debugName() + " " + name else "<no argument type>"
        } mkString (", ")
        output.append(str); //$NON-NLS-1$
      }
    } else {
      output.append("<no argument types>"); //$NON-NLS-1$
    }
    output.append(") "); //$NON-NLS-1$

    if (m.thrownExceptions != null) {
      if (m.thrownExceptions != Binding.NO_EXCEPTIONS) {
        output.append("throws "); //$NON-NLS-1$
        val s = m.thrownExceptions.map { t ⇒
          if (t != null) t.debugName() else "<no exception type>"
        }.mkString(", ")
        output.append(s)
      }
    } else {
      output.append("<no exception types>"); //$NON-NLS-1$
    }
    output.toString();
  }
  private def findMethodParameterNamesSource(m: MethodBinding, sourceType: SourceTypeBinding): Option[Array[String]] = {
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

  private def findMethodParameterNamesBinaryEnv(m: MethodBinding,
                                                rb: ReferenceBinding, nameEnvironment: INameEnvironment): Option[Array[String]] = {
    nameEnvironment.findType(rb.compoundName) match {
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
  def findMethodParamNames(m: MethodBinding, javaProject: IJavaProject) = {
    val e = m.declaringClass.erasure()
    val tpeName = e.qualifiedPackageName.mkString + "." + e.qualifiedSourceName().mkString
    try {
      val tpe = javaProject.findType(tpeName)
      tpe.getMethods() find { im ⇒
        (if (m.isConstructor())
          im.isConstructor()
        else im.getElementName == m.selector.mkString) && im.getSignature == m.signature.mkString
      } map { meth ⇒
        meth.getParameterNames()
      }
    } catch { case j: JavaModelException ⇒ None }
  }
  private def findMethodParams(m: MethodBinding, binFunc: ReferenceBinding ⇒ Option[Array[String]]): Option[Array[String]] = {
    val erasure = m.declaringClass.erasure();
    erasure match {
      case sourceType: SourceTypeBinding ⇒
        findMethodParameterNamesSource(m, sourceType)
      case rb: ReferenceBinding ⇒
        binFunc(rb)
      case _ ⇒ None
    }
  }

  def findMethodParameterNamesEnv(m: MethodBinding, nameEnvironment: INameEnvironment): Option[Array[String]] =
    findMethodParams(m, findMethodParameterNamesBinaryEnv(m, _, nameEnvironment))

}