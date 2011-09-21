package org.zaluum.nide.eclipse.integration.model

import java.util.{ Map ⇒ JMap }

import scala.collection.JavaConversions.collectionAsScalaIterable

import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.jdt.core.ICompilationUnit.NO_AST
import org.eclipse.jdt.core.compiler.CategorizedProblem
import org.eclipse.jdt.core.dom.AST
import org.eclipse.jdt.core.CompletionRequestor
import org.eclipse.jdt.core.ITypeRoot
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.WorkingCopyOwner
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.IErrorHandlingPolicy
import org.eclipse.jdt.internal.core.util.Util
import org.eclipse.jdt.internal.core.CompilationUnitElementInfo
import org.eclipse.jdt.internal.core.ASTHolderCUInfo
import org.eclipse.jdt.internal.core.CompilationUnit
import org.eclipse.jdt.internal.core.CompilationUnitProblemFinder
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.core.OpenableElementInfo
import org.eclipse.jdt.internal.core.PackageFragment
import org.zaluum.nide.eclipse.integration.MultiplexingSourceElementRequestorParser
import org.zaluum.nide.utils.ReflectionUtils

class ZaluumCompilationUnit(parent: PackageFragment, name: String, owner: WorkingCopyOwner) extends CompilationUnit(parent, name, owner) {

  override def buildStructure(info: OpenableElementInfo, pm: IProgressMonitor, newElements: JMap[_, _], _underlyingResource: IResource): Boolean = {
    var underlyingResource = _underlyingResource
    //if (!isOnBuildPath) return false
    val unitInfo = info.asInstanceOf[CompilationUnitElementInfo]
    if (getBufferManager.getBuffer(this) == null) {
      openBuffer(pm, unitInfo)
    };
    val requestor = new ZaluumCompilationUnitStructureRequestor(this, unitInfo, newElements);
    val perWorkingCopyInfo = getPerWorkingCopyInfo();
    val project = getJavaProject.asInstanceOf[JavaProject]
    var problems: java.util.HashMap[Any, Any] = null
    // Determine what kind of buildStructure
    val (createAST, resolveBindings, reconcileFlags) = info match {
      case astHolder: ASTHolderCUInfo ⇒
        def getField(str: String) = ReflectionUtils.getPrivateField(classOf[ASTHolderCUInfo], str, astHolder)
        val createAST = getField("astLevel").asInstanceOf[Int] != NO_AST
        val resolveBindings = getField("resolveBindings").asInstanceOf[Boolean]
        val reconcileFlags = getField("reconcileFlags").asInstanceOf[Int]
        problems = getField("problems").asInstanceOf[java.util.HashMap[Any, Any]]
        (createAST, resolveBindings, reconcileFlags)
      case _ ⇒
        (false, false, 0)
    }

    val computeProblems = perWorkingCopyInfo != null &&
      perWorkingCopyInfo.isActive && project != null &&
      JavaProject.hasJavaNature(project.getProject)

    // compiler options
    val options = (if (project == null) JavaCore.getOptions else project.getOptions(true)).asInstanceOf[JMap[String, String]]
    if (!computeProblems) {
      // disable task tags
      options.put(JavaCore.COMPILER_TASK_TAGS, "")
    }
    val compilerOptions = new CompilerOptions(options)
    if (project != null) {
      // TODO set classpath
    }
    // parser
    val problemFactory = new DefaultProblemFactory()
    val reporter = new ProblemReporter(new ZaluumErrorHandlingPolicy(!computeProblems), compilerOptions, new DefaultProblemFactory())

    val parser = new MultiplexingSourceElementRequestorParser(reporter, requestor, problemFactory, compilerOptions, true, !createAST)
    // update timestamp
    if (underlyingResource == null) underlyingResource = getResource()
    if (underlyingResource != null) {
      ReflectionUtils.setPrivateField(classOf[CompilationUnitElementInfo], "timestamp", unitInfo, underlyingResource.getModificationStamp())
    }
    val source: CompilationUnit = this // ??? cloneCachingContents
    var compilationUnitDeclaration: ZaluumCompilationUnitDeclaration = null
    if (computeProblems) {
      if (problems == null) {
        problems = new java.util.HashMap()
        compilationUnitDeclaration = CompilationUnitProblemFinder.process(source, parser, this.owner, problems, createAST, reconcileFlags, pm).asInstanceOf[ZaluumCompilationUnitDeclaration]
        try {
          perWorkingCopyInfo.beginReporting
          import scala.collection.JavaConversions._
          for (p ← problems.values; val ocat = Option(p.asInstanceOf[Array[CategorizedProblem]]); cat ← ocat; problem ← cat) {
            perWorkingCopyInfo.acceptProblem(problem)
          }
        } finally { perWorkingCopyInfo.endReporting }
      } else {
        compilationUnitDeclaration = CompilationUnitProblemFinder.process(source, parser, this.owner, problems, createAST, reconcileFlags, pm).asInstanceOf[ZaluumCompilationUnitDeclaration]
      }
    } else {
      compilationUnitDeclaration = parser.parseCompilationUnit(source, true, pm).asInstanceOf[ZaluumCompilationUnitDeclaration]
    }

    // create AST
    if (createAST) {
      try {
        val ast = AST.convertCompilationUnit(AST.JLS3, compilationUnitDeclaration, options, computeProblems, source, reconcileFlags, pm)
        ReflectionUtils.setPrivateField(classOf[ASTHolderCUInfo], "ast", info, ast)
      } catch {
        case e: OperationCanceledException ⇒ throw e
        case e: IllegalArgumentException ⇒
          Util.log(e, "Problem with build structure: Offset for AST node is incorrect in " //$NON-NLS-1$
            + this.getParent().getElementName() + "." + getElementName()); //$NON-NLS-1$
        case e ⇒
          Util.log(e, "Problem with build structure for " + this.getElementName()); //$NON-NLS-1$
      }
    }
    unitInfo.isStructureKnown
  }
  // No idea why it's useful
  override def cloneCachingContents() = {
    new ZaluumCompilationUnit(parent, name, owner) {
      val cachedContents = ZaluumCompilationUnit.this.getContents
      override def getContents() = cachedContents
      override def originalFromClone = ZaluumCompilationUnit.this
      override def getFileName = ZaluumCompilationUnit.this.getFileName
    }
  }
  override def getAdapter(adapter: Class[_]) = {
    if (adapter == classOf[ZaluumCompilationUnit]) this
    else super.getAdapter(adapter)
  }
  override def findPrimaryType() = {
    val typeName = withoutDotZaluum(getElementName)
    val primary = getType(typeName)
    if (primary.exists) primary else null
  }
  def withoutDotZaluum(str: String) = {
    if (str.endsWith(".zaluum")) str.dropRight(".zaluum".length) else str
  }
  override protected def codeComplete(cu: org.eclipse.jdt.internal.compiler.env.ICompilationUnit,
                                      unitToSkip: org.eclipse.jdt.internal.compiler.env.ICompilationUnit, position: Int, requestor: CompletionRequestor,
                                      owner: WorkingCopyOwner, typeRoot: ITypeRoot, monitor: IProgressMonitor) {

  }

}
class ZaluumErrorHandlingPolicy(stopOnFirst: Boolean) extends IErrorHandlingPolicy {
  def proceedOnErrors = !stopOnFirst;
  def stopOnFirstError = stopOnFirst;
}