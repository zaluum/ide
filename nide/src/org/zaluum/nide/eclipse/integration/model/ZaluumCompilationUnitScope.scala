package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.lookup.Scope
class ZaluumCompilationUnitScope(cudp: ZaluumCompilationUnitDeclaration, lookupEnvironment: LookupEnvironment) extends CompilationUnitScope(cudp, lookupEnvironment)  {
  override protected def buildClassScope(parent: Scope, typeDecl: TypeDeclaration) = {
    new ZaluumClassScope(parent, typeDecl)
  }
  def getExpectedPackageName = this.referenceContext.compilationResult.compilationUnit.getPackageName();
  def cud = referenceContext.asInstanceOf[ZaluumCompilationUnitDeclaration]

}