package org.zaluum.nide.eclipse.integration.model

import java.io.File;
import java.lang.reflect.Field;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.JarFile;

import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.batch.FileSystem;
import org.eclipse.jdt.internal.compiler.batch.FileSystem.Classpath;
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit;
import org.eclipse.jdt.internal.compiler.env.INameEnvironment;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.core.builder.BatchImageBuilder;
import org.eclipse.jdt.internal.core.builder.BuildNotifier;
import org.eclipse.jdt.internal.core.builder.SourceFile;

class ZaluumMockParser(var requestor : Object , val options : CompilerOptions , val problemReporter : ProblemReporter , allowTransforms : Boolean ) {
  def this (options : CompilerOptions, problemReporter : ProblemReporter , allowTransforms : Boolean ) {
    this(null, options, problemReporter, allowTransforms);
  }
  def dietParse(sourceUnit : ICompilationUnit, compilationResult: CompilationResult) : CompilationUnitDeclaration = {
    val sourceCode = sourceUnit.getContents
    compilationResult.lineSeparatorPositions = Array(1,1)
    val zcud = new ZaluumCompilationUnitDeclaration(problemReporter,compilationResult, 
        sourceCode.size, /*groovyCU,groovySU,*/ options)
    zcud.processToPhase(0)
    zcud.populateCompilationUnitDeclaration()
    for (decl <- zcud.types) {
      val zDecl = decl.asInstanceOf[ZaluumTypeDeclaration]
      //resolver.record(zDecl)
    }
    zcud
    
  }
}