package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.parser.Parser
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.Analyzer
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding
import org.zaluum.nide.compiler.TreeToClass
import org.zaluum.nide.compiler.ByteCodeGen
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.zaluum.nide.compiler.Reporter
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities

class ZaluumTypeDeclaration(compilationResult: CompilationResult, val b: BoxDef) extends TypeDeclaration(compilationResult) {
  override def parseMethods(parser: Parser, unit: CompilationUnitDeclaration) {
    //nop
  }
  override def isScannerUsableOnThisDeclaration = false
  def zaluumScope = scope.asInstanceOf[ZaluumClassScope]
  var a: Analyzer = _
  override def resolve() {
    super.resolve()
    val reporter = new Reporter() {
      override def report(str: String, mark: Option[Int] = None) {
        super.report(str, mark)
        ignoreFurtherInvestigation = true
        createProblem(str, mark.getOrElse(-1))
      }
    }
    a = new Analyzer(reporter, b)
    a.runNamer()
    try {
      val scope = this.scope.asInstanceOf[ZaluumClassScope]
      b.sym.javaScope = scope //  a bit ugly...
      a.runResolve(this, zaluumScope)
      a.runCheck()
      checkZaluumLibraryPresent()
    } catch { case e ⇒ e.printStackTrace }
  }
  def checkZaluumLibraryPresent() {
    // add a descriptive error to help users
    val errors = compilationResult.getErrors
    if (errors != null && errors.exists(_.getMessage == "org.zaluum cannot be resolved to a type")) {
      createProblem("Zaluum Runtime library is not in the classpath. Add org.zaluum.runtime jar to fix this problem.", 1)
    }
  }
  def createProblem(msg: String, line: Int) {
    val p = new DefaultProblemFactory().createProblem(compilationResult.fileName, 0, Array(msg), 0, Array(msg), ProblemSeverities.Error, 0, 1, line, 1)
    compilationResult.record(p, this)
  }
}