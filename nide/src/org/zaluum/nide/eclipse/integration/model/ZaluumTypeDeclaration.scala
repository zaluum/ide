package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.parser.Parser
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.zaluum.nide.compiler.BoxDef

class ZaluumTypeDeclaration(compilationResult:CompilationResult,val b:BoxDef) extends TypeDeclaration(compilationResult) {
  override def parseMethods(parser:Parser, unit:CompilationUnitDeclaration) {
    //nop
  }
  override def isScannerUsableOnThisDeclaration = false
  def zaluumScope = scope.asInstanceOf[ZaluumClassScope]
}