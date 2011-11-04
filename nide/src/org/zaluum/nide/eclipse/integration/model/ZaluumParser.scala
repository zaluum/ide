package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.CompilationResult

class ZaluumParser(val options: CompilerOptions, val problemReporter: ProblemReporter) {
  def dietParse(sourceUnit: ICompilationUnit, compilationResult: CompilationResult): CompilationUnitDeclaration = {
    val sourceCode = sourceUnit.getContents
    compilationResult.lineSeparatorPositions = Array(1, 1)
    if (sourceCode == null) return null
    val zcud = new ZaluumCompilationUnitDeclaration(problemReporter, compilationResult,
      sourceCode.size, sourceUnit, options)
    zcud.populateCompilationUnitDeclaration()
    zcud

  }
}