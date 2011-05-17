package org.zaluum.nide.eclipse.integration.model

import org.eclipse.jdt.internal.core.CompilationUnitElementInfo
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.internal.compiler.SourceElementParser
import org.eclipse.jdt.internal.core.CompilationUnitStructureRequestor

// make constructor accessible
class ZaluumCompilationUnitStructureRequestor (unit:ICompilationUnit, unitInfo : CompilationUnitElementInfo, newElements : java.util.Map[_,_]) extends CompilationUnitStructureRequestor(unit,unitInfo,newElements){
  def setParser(parser:SourceElementParser) {
    this.parser=parser
  }
}