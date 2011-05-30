package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.core.dom.ASTParser
import org.zaluum.nide.compiler.Tree
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.internal.core.BasicCompilationUnit

class ZaluumASTParser(level:Int,tree: Tree) extends ASTParser(level) {
  override def createBasicCompilationUnit(sourceUnit : org.eclipse.jdt.internal.compiler.env.ICompilationUnit ) = {
    new PreParsedZaluumCompilationUnit(sourceUnit.getContents(), sourceUnit.getPackageName(), new String(sourceUnit.getFileName()), this.project, tree);
  }
}
  

class PreParsedZaluumCompilationUnit(contents:Array[Char], packageName:Array[Array[Char]], fileName: String, element:IJavaElement, val tree:Tree) 
  extends BasicCompilationUnit(contents,packageName,fileName,element) 
