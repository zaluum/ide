package org.zaluum.nide.eclipse.integration;

import org.zaluum.nide.eclipse.integration.model.ZaluumMockParser
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.core.util.CommentRecorderParser;

class MultiplexingCommentRecorderParser(requestor: Object , val compilerOptions:CompilerOptions , problemReporter:ProblemReporter ,
      optimizeStringLiterals:Boolean , var allowTransforms:Boolean ) extends 
  CommentRecorderParser(problemReporter, optimizeStringLiterals) {

  
	val zaluumParser = new model.ZaluumMockParser(requestor, compilerOptions, problemReporter, allowTransforms);

	def this (requestor : Object , compilerOptions : CompilerOptions , problemReporter : ProblemReporter , optimizeStringLiterals : Boolean ) {
		this(requestor, compilerOptions, problemReporter, optimizeStringLiterals, true);
	}

	override def dietParse(sourceUnit:ICompilationUnit , compilationResult:CompilationResult ) : CompilationUnitDeclaration = {
		if (ContentTypeUtils.isZaluumLikeFileName(sourceUnit.getFileName())) {
			// even though the Java scanner is not used, its contents can be asked for.
			if (this.scanner != null) {
				this.scanner.setSource(sourceUnit.getContents());
			}
			// FIXASC Is it ok to use a new parser here everytime? If we don't we sometimes recurse back into the first one
			// FIXASC ought to reuse to ensure types end up in same groovy CU
			return new ZaluumMockParser(this.zaluumParser.requestor, this.zaluumParser.options,
					this.zaluumParser.problemReporter, allowTransforms).dietParse(sourceUnit, compilationResult);
		} else {
			return super.dietParse(sourceUnit, compilationResult);
		}
	}

	override def reset() {
		//groovyParser.reset();
	}
}
