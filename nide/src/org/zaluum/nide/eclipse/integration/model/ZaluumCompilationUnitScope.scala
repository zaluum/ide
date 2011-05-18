package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.Scope

class ZaluumCompilationUnitScope(cud: ZaluumCompilationUnitDeclaration, lookupEnvironment : LookupEnvironment) extends CompilationUnitScope(cud,lookupEnvironment) {
  override protected def buildClassScope(parent:Scope, typeDecl : TypeDeclaration ) = {
    new ZaluumClassScope(parent,typeDecl)
  }
}