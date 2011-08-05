package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.lookup.ClassScope
import org.eclipse.jdt.internal.compiler.lookup.Scope

class ZaluumClassScope(parent: Scope, typeDecl: TypeDeclaration) extends ClassScope(parent, typeDecl) {
  override protected def buildClassScope(parent: Scope, typeDecl: TypeDeclaration) = {
    new ZaluumClassScope(parent, typeDecl)
  }
}