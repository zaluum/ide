/**
 * *****************************************************************************
 * Copyright (c) 2009 Codehaus.org, SpringSource, and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Andy Clement        - Initial API and implementation
 *     Andrew Eisenberg - Additional work
 * *****************************************************************************
 */
package org.zaluum.nide.eclipse.integration;

import org.zaluum.nide.eclipse.integration.model.ZaluumMockParser
import java.util.Collections;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.IProblemFactory;
import org.eclipse.jdt.internal.compiler.SourceElementNotifier;
import org.eclipse.jdt.internal.compiler.SourceElementParser;
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.compiler.util.HashtableOfObjectToInt;
import org.eclipse.jdt.internal.compiler.ISourceElementRequestor;

/**
 * The multiplexing parser can delegate file parsing to multiple parsers. In this scenario it subtypes 'Parser' (which is the Java
 * parser) but is also aware of a groovy parser. Depending on what kind of file is to be parsed, it will invoke the relevant parser.
 *
 * @author Andrew Eisenberg
 */
class MultiplexingSourceElementRequestorParser(
  problemReporter: ProblemReporter,
  requestor: ISourceElementRequestor,
  problemFactory: IProblemFactory,
  options: CompilerOptions,
  reportLocalDeclarations: Boolean,
  optimizeStringLiterals: Boolean) extends SourceElementParser(requestor, problemFactory, options, reportLocalDeclarations, optimizeStringLiterals) {
  reportReferenceInfo = false
  notifier = new SourceElementNotifier(requestor, reportLocalDeclarations);
  val parser = new ZaluumMockParser(requestor, this.options, problemReporter, false)
  override def parseCompilationUnit(unit: ICompilationUnit, fullParse: Boolean, pm: IProgressMonitor): CompilationUnitDeclaration = {

    if (ContentTypeUtils.isZaluumLikeFileName(unit.getFileName())) {
      // ASSUMPTIONS:
      // 1) there is no difference between a diet and full parse in the groovy works, so can ignore the fullParse parameter
      // 2) parsing is for the entire CU (ie- from character 0, to unit.getContents().length)
      // 3) nodesToCategories map is not necessary. I think it has something to do with JavaDoc, but not sure

      val compilationResult = new CompilationResult(unit, 0, 0, this.options.maxProblemsPerUnit);

      // FIXASC Is it ok to use a new parser here everytime? If we don't we sometimes recurse back into the first one
      // FIXASC ought to reuse to ensure types end up in same groovy CU
      val cud = new ZaluumMockParser(this.parser.requestor, this.options, problemReporter, false)
        .dietParse(unit, compilationResult);

      val sourceEnds = createSourceEnds(cud);

      notifier.notifySourceElementRequestor(cud, 0, unit.getContents().length, reportReferenceInfo, sourceEnds,
        /* We don't care about the @category tag, so pass empty map */ Collections.EMPTY_MAP);
      return null;
    } else {
      return super.parseCompilationUnit(unit, fullParse, pm);
    }
  }

  override def dietParse(sourceUnit: ICompilationUnit, compilationResult: CompilationResult): CompilationUnitDeclaration = {
    if (ContentTypeUtils.isZaluumLikeFileName(sourceUnit.getFileName())) {
      return parser.dietParse(sourceUnit, compilationResult);
    } else {
      return super.dietParse(sourceUnit, compilationResult);
    }
  }

  // FIXASC This should be calculated in GroovyCompilationUnitDeclaration
  private def createSourceEnds(cDecl : CompilationUnitDeclaration ) : HashtableOfObjectToInt = {
		val table = new HashtableOfObjectToInt();
		if (cDecl.types != null) {
			for (tDecl <- cDecl.types) {
				createSourceEndsForType(tDecl, table);
			}
		}
		return table;
	}

  override def reset() {
    //parser.reset();
  }

  // FIXASC This should be calculated in GroovyCompilationUnitDeclaration
  private def createSourceEndsForType(tDecl : TypeDeclaration , table : HashtableOfObjectToInt ) {
		table.put(tDecl, tDecl.sourceEnd);
		if (tDecl.fields != null) {
			for (fDecl <- tDecl.fields) {
				table.put(fDecl, fDecl.sourceEnd);
			}
		}
		if (tDecl.methods != null) {
			for (mDecl <- tDecl.methods) {
				table.put(mDecl, mDecl.sourceEnd);
			}
		}
		if (tDecl.memberTypes != null) {
			for (innerTDecl <- tDecl.memberTypes) {
				createSourceEndsForType(innerTDecl, table);
			}
		}
	}

}
