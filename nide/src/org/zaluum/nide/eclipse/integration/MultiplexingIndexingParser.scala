/**
 * *****************************************************************************
 * Copyright (c) 2009 Codehaus.org, SpringSource, and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Andrew Eisenberg - Initial API and implementation
 *     Andy Clement     - Additional work
 * *****************************************************************************
 */
package org.zaluum.nide.eclipse.integration;

import java.util.Collections
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.eclipse.jdt.internal.compiler.IProblemFactory
import org.eclipse.jdt.internal.compiler.ISourceElementRequestor
import org.eclipse.jdt.internal.compiler.SourceElementNotifier
import org.eclipse.jdt.internal.compiler.SourceElementParser
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.util.HashtableOfObjectToInt
import org.eclipse.jdt.internal.core.search.indexing.IndexingParser
import model.ZaluumParser
import model.ZaluumCompilationUnit
import model.ZaluumCompilationUnitDeclaration
import model.JDTInternalUtils
import org.zaluum.annotation.Box
import org.eclipse.jdt.internal.core.jdom.CompilationUnit
import org.eclipse.jdt.internal.core.search.BasicSearchEngine

/**
 * @author Andrew Eisenberg
 * @created Aug 27, 2009
 *
 */
class MultiplexingIndexingParser(r: ISourceElementRequestor,
  problemFactory: IProblemFactory,
  options: CompilerOptions,
  reportLocalDeclarations: Boolean,
  optimizeStringLiterals: Boolean,
  useSourceJavadocParser: Boolean) extends IndexingParser(r, problemFactory, options, reportLocalDeclarations, optimizeStringLiterals, useSourceJavadocParser) {

  override def parseCompilationUnit(unit: ICompilationUnit, fullParse: Boolean, pm: IProgressMonitor): CompilationUnitDeclaration = {
    if (ContentTypeUtils.isZaluumLikeFileName(unit.getFileName())) {

      // ASSUMPTIONS:
      // 1) there is no difference between a diet and full parse in the groovy works, so can ignore the fullParse parameter
      // 2) parsing is for the entire CU (ie- from character 0, to unit.getContents().length)
      // 3) nodesToCategories map is not necessary. I think it has something to do with JavaDoc, but not sure
      val compilationResult = new CompilationResult(unit, 0, 0, this.options.maxProblemsPerUnit);
      // FIXASC Is it ok to use a new parser here everytime? If we don't we sometimes recurse back into the first one
      val cud = new ZaluumParser(requestor, this.options,
        problemReporter).dietParse(unit, compilationResult).asInstanceOf[ZaluumCompilationUnitDeclaration];
      import JDTInternalUtils._
      val sourceEnds = createSourceEnds(cud);
      // TODO FAKED index entries
      requestor.acceptPackage(cud.currentPackage)
      requestor.acceptTypeReference(stringToA(cud.fqName), 40, 41)
      requestor.acceptAnnotationTypeReference(stringToA(classOf[Box].getName), 42, 43)
      notifier.notifySourceElementRequestor(cud, 0, unit.getContents().length, reportLocalDeclarations, sourceEnds,
        /* We don't care about the @category tag, so pass empty map */ Collections.EMPTY_MAP);
      cud;
    } else {
      super.parseCompilationUnit(unit, fullParse, pm);
    }
  }

  // FIXASC this code is copied from MultiplexingSourceElementParser. Should combine
  // FIXASC This should be calculated in GroovyCompilationUnitDeclaration
  private def createSourceEnds(cDecl: CompilationUnitDeclaration): HashtableOfObjectToInt = {
    val table = new HashtableOfObjectToInt();
    if (cDecl.types != null) {
      for (tDecl ← cDecl.types) {
        createSourceEndsForType(tDecl, table);
      }
    }
    return table;
  }

  // FIXASC this code is copied from MultiplexingSourceElementParser. Should combine
  // FIXASC This should be calculated in GroovyCompilationUnitDeclaration
  private def createSourceEndsForType(tDecl: TypeDeclaration, table: HashtableOfObjectToInt) {
    table.put(tDecl, tDecl.sourceEnd);
    if (tDecl.fields != null) {
      for (fDecl ← tDecl.fields)
        table.put(fDecl, fDecl.sourceEnd);
    }
    if (tDecl.methods != null) {
      for (mDecl ← tDecl.methods)
        table.put(mDecl, mDecl.sourceEnd);
    }
    if (tDecl.memberTypes != null) {
      for (innerTDecl ← tDecl.memberTypes)
        createSourceEndsForType(innerTDecl, table);
    }
  }

}
