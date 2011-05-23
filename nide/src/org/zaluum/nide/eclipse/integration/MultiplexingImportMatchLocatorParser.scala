/*
 * Copyright 2003-2009 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.zaluum.nide.eclipse.integration;

import org.zaluum.nide.eclipse.integration.model.ZaluumParser
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.core.search.matching.ImportMatchLocatorParser;
import org.eclipse.jdt.internal.core.search.matching.MatchLocator;

/**
 * @author Andrew Eisenberg
 * @created Nov 17, 2009
 * 
 */
class MultiplexingImportMatchLocatorParser( problemReporter:ProblemReporter,  locator:MatchLocator) extends ImportMatchLocatorParser(problemReporter, locator) {
  val zaluumParser = new ZaluumParser(null,locator.options,problemReporter)
  
	override def dietParse(sourceUnit : ICompilationUnit , compilationResult : CompilationResult ) : CompilationUnitDeclaration = {
		if (ContentTypeUtils.isZaluumLikeFileName(sourceUnit.getFileName())) {
			// FIXASC Is it ok to use a new parser here everytime? If we don't we sometimes recurse back into the first one
			// FIXASC ought to reuse to ensure types end up in same groovy CU
			return new ZaluumParser(null, zaluumParser.options, zaluumParser.problemReporter)
			  .dietParse(sourceUnit, compilationResult);
		} else {
		  super.dietParse(sourceUnit, compilationResult);
		}
	}

}
