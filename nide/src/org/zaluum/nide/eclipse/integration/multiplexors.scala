package org.zaluum.nide.eclipse.integration

import java.util.Collections
import model.JDTInternalUtils
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.internal.compiler.ast.{ TypeDeclaration, CompilationUnitDeclaration }
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.parser.Parser
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.util.HashtableOfObjectToInt
import org.eclipse.jdt.internal.compiler.{ SourceElementNotifier, SourceElementParser, IProblemFactory, ISourceElementRequestor, CompilationResult }
import org.eclipse.jdt.internal.core.search.indexing.IndexingParser
import org.eclipse.jdt.internal.core.search.matching.{ MatchLocatorParser, MatchLocator, ImportMatchLocatorParser }
import org.eclipse.jdt.internal.core.util.CommentRecorderParser
import org.zaluum.annotation.Box
import org.zaluum.nide.eclipse.integration.model.{ ZaluumCompilationUnitDeclaration, ZaluumParser }

trait ParserParams {
  def compilerOptions: CompilerOptions
  def problemReporterAccessor: ProblemReporter
}
trait ZaluumDietParse extends Parser with ParserParams {
  override def dietParse(sourceUnit: ICompilationUnit, compilationResult: CompilationResult): CompilationUnitDeclaration = {
    if (ContentTypeUtils.isZaluumLikeFileName(sourceUnit.getFileName())) {
      // even though the Java scanner is not used, its contents can be asked for.
      if (this.scanner != null) {
        this.scanner.setSource(sourceUnit.getContents());
      }
      return new ZaluumParser(compilerOptions, problemReporterAccessor).dietParse(sourceUnit, compilationResult);
    } else {
      super.dietParse(sourceUnit, compilationResult);
    }
  }
}
class MultiplexingParser(
  val compilerOptions: CompilerOptions,
  val problemReporterAccessor: ProblemReporter,
  optimizeStringLiterals: Boolean)
  extends Parser(problemReporterAccessor, optimizeStringLiterals)
  with ZaluumDietParse

class MultiplexingCommentRecorderParser(
  val compilerOptions: CompilerOptions,
  val problemReporterAccessor: ProblemReporter,
  optimizeStringLiterals: Boolean)
  extends CommentRecorderParser(problemReporterAccessor, optimizeStringLiterals)
  with ZaluumDietParse

class MultiplexingImportMatchLocatorParser(
  val problemReporterAccessor: ProblemReporter,
  locator: MatchLocator)
  extends ImportMatchLocatorParser(problemReporterAccessor, locator) with ZaluumDietParse {

  def compilerOptions = locator.options
}

class MultiplexingMatchLocatorParser(
  val problemReporterAccessor: ProblemReporter,
  locator: MatchLocator)
  extends MatchLocatorParser(problemReporterAccessor, locator) with ZaluumDietParse {
  def compilerOptions = locator.options
}

trait ZaluumParseCompilationUnit extends SourceElementParser with FixEnds with ParserParams {
  def reportLocalDeclarationsAccessor: Boolean
  def notifierAccessor: SourceElementNotifier
  override def parseCompilationUnit(unit: ICompilationUnit, fullParse: Boolean, pm: IProgressMonitor): CompilationUnitDeclaration = {
    if (ContentTypeUtils.isZaluumLikeFileName(unit.getFileName())) {

      val compilationResult = new CompilationResult(unit, 0, 0, compilerOptions.maxProblemsPerUnit);
      val cud = new ZaluumParser(compilerOptions, problemReporterAccessor).dietParse(unit, compilationResult).asInstanceOf[ZaluumCompilationUnitDeclaration];
      val sourceEnds = createSourceEnds(cud);
      withCUD(cud)
      // TODO FAKED index entries
      notifierAccessor.notifySourceElementRequestor(cud, 0, unit.getContents().length, reportLocalDeclarationsAccessor, sourceEnds,
        /* We don't care about the @category tag, so pass empty map */ Collections.EMPTY_MAP);
      cud;
    } else {
      super.parseCompilationUnit(unit, fullParse, pm);
    }
  }
  def withCUD(cud: ZaluumCompilationUnitDeclaration) {}
}
class MultiplexingIndexingParser(r: ISourceElementRequestor,
  problemFactory: IProblemFactory,
  val compilerOptions: CompilerOptions,
  val reportLocalDeclarationsAccessor: Boolean,
  optimizeStringLiterals: Boolean,
  useSourceJavadocParser: Boolean) extends IndexingParser(r, problemFactory, compilerOptions, reportLocalDeclarationsAccessor, optimizeStringLiterals, useSourceJavadocParser)
  with ZaluumParseCompilationUnit {
  def notifierAccessor = notifier
  def problemReporterAccessor = problemReporter
  override def withCUD(cud: ZaluumCompilationUnitDeclaration) = {
    import JDTInternalUtils._
    requestor.acceptPackage(cud.currentPackage)
    requestor.acceptTypeReference(stringToA(cud.fqName), 0, 1)
    requestor.acceptAnnotationTypeReference(stringToA(classOf[Box].getName), 2, 3)
  }
}

class MultiplexingSourceElementRequestorParser(
  val problemReporterAccessor: ProblemReporter,
  requestor: ISourceElementRequestor,
  problemFactory: IProblemFactory,
  val compilerOptions: CompilerOptions,
  val reportLocalDeclarationsAccessor: Boolean,
  optimizeStringLiterals: Boolean)
  extends SourceElementParser(requestor, problemFactory, compilerOptions, reportLocalDeclarationsAccessor, optimizeStringLiterals)
  with ZaluumParseCompilationUnit with ZaluumDietParse {
  def notifierAccessor = notifier
  reportReferenceInfo = false
  notifier = new SourceElementNotifier(requestor, reportLocalDeclarationsAccessor);

}
trait FixEnds {
  protected def createSourceEnds(cDecl: CompilationUnitDeclaration): HashtableOfObjectToInt = {
    val table = new HashtableOfObjectToInt();
    if (cDecl.types != null) {
      for (tDecl ← cDecl.types) {
        createSourceEndsForType(tDecl, table);
      }
    }
    return table;
  }
  protected def createSourceEndsForType(tDecl: TypeDeclaration, table: HashtableOfObjectToInt) {
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