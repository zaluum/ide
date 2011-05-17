package org.zaluum.nide.eclipse.integration
import org.eclipse.jdt.internal.compiler.ISourceElementRequestor
import org.eclipse.jdt.internal.compiler.ast.ImportReference

class ZaluumIndexingVisitor(val requestor: ISourceElementRequestor) {
  def doVisit(pkg : ImportReference) {
    //requestor.a
  }
}