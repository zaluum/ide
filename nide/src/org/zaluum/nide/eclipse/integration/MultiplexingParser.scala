/*******************************************************************************
 * Copyright (c) 2009 Codehaus.org, SpringSource, and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Andy Clement        - Initial API and implementation
 *     Andrew Eisenberg - Additional work
 *******************************************************************************/
package org.zaluum.nide.eclipse.integration;

import org.zaluum.nide.eclipse.integration.model.ZaluumParser
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.parser.Parser;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;

/**
 * The multiplexing parser can delegate file parsing to multiple parsers. In this scenario it subtypes 'Parser' (which is the Java
 * parser) but is also aware of a groovy parser. Depending on what kind of file is to be parsed, it will invoke the relevant parser.
 * 
 * @author Andy Clement
 */
class MultiplexingParser(requestor : Object , compilerOptions : CompilerOptions , problemReporter : ProblemReporter ,
      optimizeStringLiterals : Boolean ) extends Parser(problemReporter, optimizeStringLiterals) {

  val zaluumParser = new ZaluumParser(requestor, compilerOptions, problemReporter);
	
  override def dietParse(sourceUnit : ICompilationUnit , compilationResult : CompilationResult ) : CompilationUnitDeclaration = {
		if (ContentTypeUtils.isZaluumLikeFileName(sourceUnit.getFileName())) {
		  println ("multiplex parse " + sourceUnit.getFileName.mkString)
			return zaluumParser.dietParse(sourceUnit, compilationResult);
		} else {
			return super.dietParse(sourceUnit, compilationResult);
		}
	}

	override def reset() {
		//groovyParser.reset();
	}
}
