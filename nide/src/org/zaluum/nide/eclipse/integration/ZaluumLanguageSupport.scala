package org.zaluum.nide.eclipse.integration

import org.codehaus.jdt.groovy.integration.EventHandler
import org.codehaus.jdt.groovy.integration.LanguageSupport
import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.search.SearchMatch
import org.eclipse.jdt.core.search.SearchPattern
import org.eclipse.jdt.core.search.SearchRequestor
import org.eclipse.jdt.core.search.TypeDeclarationMatch
import org.eclipse.jdt.core.search.TypeReferenceMatch
import org.eclipse.jdt.core.WorkingCopyOwner
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.parser.Parser
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.eclipse.jdt.internal.compiler.DefaultErrorHandlingPolicies
import org.eclipse.jdt.internal.compiler.IProblemFactory
import org.eclipse.jdt.internal.compiler.ISourceElementRequestor
import org.eclipse.jdt.internal.compiler.SourceElementParser
import org.eclipse.jdt.internal.core.search.indexing.IndexingParser
import org.eclipse.jdt.internal.core.search.matching.ImportMatchLocatorParser
import org.eclipse.jdt.internal.core.search.matching.MatchLocator
import org.eclipse.jdt.internal.core.search.matching.MatchLocatorParser
import org.eclipse.jdt.internal.core.search.matching.PossibleMatch
import org.eclipse.jdt.internal.core.search.matching.TypeDeclarationPattern
import org.eclipse.jdt.internal.core.search.matching.TypeReferencePattern
import org.eclipse.jdt.internal.core.util.Util
import org.eclipse.jdt.internal.core.BinaryType
import org.eclipse.jdt.internal.core.CompilationUnit
import org.eclipse.jdt.internal.core.PackageFragment
import org.eclipse.text.edits.TextEdit
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Serializer
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitDeclaration
import org.zaluum.nide.eclipse.ZaluumNature
import model.ZaluumCompilationUnit
import org.eclipse.jdt.core.search.IJavaSearchScope

class ZaluumLanguageSupport extends LanguageSupport {

  def getParser(requestor: Object, compilerOptions: CompilerOptions, problemReporter: ProblemReporter, parseLiteralExpressionsAsConstants: Boolean, variant: Int): Parser = {
    if (variant == 1)
      new MultiplexingParser(compilerOptions, problemReporter, parseLiteralExpressionsAsConstants);
    else
      new MultiplexingCommentRecorderParser(compilerOptions, problemReporter,
        parseLiteralExpressionsAsConstants);

  }

  def getIndexingParser(requestor: ISourceElementRequestor, problemFactory: IProblemFactory, options: CompilerOptions, reportLocalDeclarations: Boolean,
                        optimizeStringLiterals: Boolean, useSourceJavadocParser: Boolean): IndexingParser = {
    new MultiplexingIndexingParser(requestor, problemFactory, options, reportLocalDeclarations, optimizeStringLiterals,
      useSourceJavadocParser);
  }

  def getMatchLocatorParserParser(problemReporter: ProblemReporter, locator: MatchLocator): MatchLocatorParser = {
    new MultiplexingMatchLocatorParser(problemReporter, locator);
  }

  def getImportMatchLocatorParserParser(problemReporter: ProblemReporter, locator: MatchLocator): ImportMatchLocatorParser = {
    new MultiplexingImportMatchLocatorParser(problemReporter, locator);
  }

  def getSourceElementParser(requestor: ISourceElementRequestor, problemFactory: IProblemFactory,
                             options: CompilerOptions, reportLocalDeclarations: Boolean, optimizeStringLiterals: Boolean,
                             useSourceJavadocParser: Boolean): SourceElementParser = {
    val problemReporter = new ProblemReporter(DefaultErrorHandlingPolicies.proceedWithAllProblems(), options,
      new DefaultProblemFactory());
    new MultiplexingSourceElementRequestorParser(problemReporter, requestor, problemFactory, options,
      reportLocalDeclarations, optimizeStringLiterals);
  }

  def newCompilationUnit(parent: PackageFragment, name: String, owner: WorkingCopyOwner): CompilationUnit = {
    // should use a content type here
    if (ContentTypeUtils.isZaluumLikeFileName(name)) {
      return new ZaluumCompilationUnit(parent, name, owner);
    } else {
      return new CompilationUnit(parent, name, owner);
    }
  }
  // XXX when is it used?
  def newCompilationUnitDeclaration(unit: ICompilationUnit, problemReporter: ProblemReporter, compilationResult: CompilationResult, sourceLength: Int): CompilationUnitDeclaration = {
    if (ContentTypeUtils.isZaluumLikeFileName(compilationResult.getFileName())) {
      compilationResult.lineSeparatorPositions = Array(1, 1)
      new ZaluumCompilationUnitDeclaration(problemReporter, compilationResult, sourceLength, unit, null)
    } else {
      new CompilationUnitDeclaration(problemReporter, compilationResult, sourceLength);
    }
  }

  /**
   * Determines if the project requires special languages support.
   *
   * @param project the project to analyze
   *
   * @return true iff the project passed in has a nature that is considered interesting
   * for this language support
   */
  def isInterestingProject(project: IProject): Boolean = {
    ZaluumNature.hasZaluumNature(project)
  }

  /**
   * Determines if fileName is a source file, and takes into account
   * whether or not this project requires special language support.
   *
   * @param fileName the name of the file to analyze
   * @param isInterestingProject true if this project requires special
   * language support, or if it should be treated like a regular
   * Java project
   *
   *  @return true iff fileName is considered a Java-like file and should
   *  be passed to the compiler to produce byte code
   */
  def isSourceFile(fileName: String, isInterestingProject: Boolean): Boolean = {
    if (isInterestingProject) {
      Util.isJavaLikeFileName(fileName);
    } else {
      Util.isJavaLikeFileName(fileName) && !fileName.endsWith(".zaluum");
      // return ContentTypeUtils.isJavaLikeButNotGroovyLikeExtension(fileName);
    }
  }

  /**
   * Determines if the file name requires special language support.  This method does not
   * examine the project nature and so will return true if the file name is interesting
   * regardless of whether or not the containing project itself is interesting.
   *
   * @param fileName
   * @return true iff the file name is one that requires special language support.
   */
  def isInterestingSourceFile(fileName: String): Boolean = {
    ContentTypeUtils.isZaluumLikeFileName(fileName);
  }

  /**
   * Maybe perform a search for the possible match using special language support.
   * Returns true if the search was completed by the special language support.
   * Even if the search document requires special language support
   * (i.e., {@link LanguageSupport#isInterestingSourceFile(String)}  returns true),
   * this method may not perform the search.  The kind of search pattern will determine
   * if a special search is required.
   * The results of the search are sent to the SearchRequestor that is passed in.
   * @param possibleMatch the possible match to look for
   * @param pattern
   * @param requestor the requestor to send any completed search results to
   * @return true iff the search was performed
   */
  def maybePerformDelegatedSearch(possibleMatch: PossibleMatch, pattern: SearchPattern, requestor: SearchRequestor) = {
    /* The standard JDT part uses the matchlocatorparser to call 
     * this.patternLocator.match(paramReferences[i], this.nodeSet);
     * We cannot reuse this because we are not reusing the jdt parser
     * 
     * Indexing Parser requestor creates possibleMatches
     * When searching MatchLocatorParser are created. They should invoke
     * this.patternLocator.match(paramReferences[i], this.nodeSet);
     * MatchLocator#report converts to requestor.acceptSearchMatch
     * Here we are bypassing the patternLocator.match and reporting directly 
     * with the requestor
     * */
    if (possibleMatch.openable != null && possibleMatch.openable.exists()) {
      val participant = possibleMatch.document.getParticipant()
      possibleMatch.openable match {
        // TODO FAKED search results
        case z: ZaluumCompilationUnit ⇒
          pattern match {
            case dec: TypeDeclarationPattern ⇒
              val tpe = z.findPrimaryType
              if (dec.simpleName.mkString.equalsIgnoreCase(tpe.getElementName)) {
                requestor.acceptSearchMatch(new TypeDeclarationMatch(z, SearchMatch.A_ACCURATE, 0, 0, participant, z.getResource()));
                true
              } else false

            case ref: TypeReferencePattern ⇒
              if (ref.getIndexKey.mkString.equalsIgnoreCase("box")) {
                requestor.acceptSearchMatch(new TypeReferenceMatch(z.findPrimaryType, SearchMatch.A_ACCURATE, 0, 0, false, participant, z.getResource()))
                true
              } else false
            case _ ⇒ false
          }
      }
    } else false
    false
  }

  def getEventHandler(): EventHandler = new ZaluumEventHandler()

  def filterNonSourceMembers(binaryType: BinaryType) {}
  def expandSearchScope(scope: IJavaSearchScope, pattern: SearchPattern, requestor: SearchRequestor) =
    scope
  def updateContent(cu: org.eclipse.jdt.core.ICompilationUnit, destPackageName: Array[String], currPackageName: Array[String],
                    newName: String): TextEdit = {
    val packageDecls = cu.getPackageDeclarations();
    val doPackage = !Util.equalArraysOrNull(currPackageName.asInstanceOf[Array[Object]], destPackageName.asInstanceOf[Array[Object]])
    val doName = newName != null;
    import org.zaluum.nide.compiler.{ Name, Serializer }
    if (doPackage || doName) {
      cu match {
        case c: CompilationUnit ⇒
          val s = c.getSource
          val name = if (newName != null) newName else cu.getElementName.split('.').last
          val tree = org.zaluum.nide.compiler.Parser.readTree(s, Name(name));
          val refactored = tree.copy(name = Name(name), pkg = Name(destPackageName.mkString(".")))
          val str = Serializer.writeToIsoString(Serializer.proto(refactored))
          return new org.eclipse.text.edits.ReplaceEdit(0, s.length, str);
        case _ ⇒
      }
    }
    null;
  }
}