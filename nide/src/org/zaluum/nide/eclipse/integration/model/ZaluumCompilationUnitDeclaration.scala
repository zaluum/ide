package org.zaluum.nide.eclipse.integration.model

import scala.collection.mutable.Buffer
import org.eclipse.jdt.core.compiler.CategorizedProblem
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.ast.SingleMemberAnnotation
import org.eclipse.jdt.internal.compiler.ast.MarkerAnnotation
import org.eclipse.jdt.internal.compiler.ASTVisitor
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.Annotation
import org.eclipse.jdt.internal.compiler.ast.AnnotationMethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.Argument
import org.eclipse.jdt.internal.compiler.ast.ArrayInitializer
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference
import org.eclipse.jdt.internal.compiler.ast.ClassLiteralAccess
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration
import org.eclipse.jdt.internal.compiler.ast.ImportReference
import org.eclipse.jdt.internal.compiler.ast.Javadoc
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference
import org.eclipse.jdt.internal.compiler.ast.ParameterizedSingleTypeReference
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference
import org.eclipse.jdt.internal.compiler.ast.StringLiteral
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.ast.TypeParameter
import org.eclipse.jdt.internal.compiler.ast.TypeReference
import org.eclipse.jdt.internal.compiler.ast.Wildcard
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.impl.IrritantSet
import org.eclipse.jdt.internal.compiler.lookup.BlockScope
import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope
import org.eclipse.jdt.internal.compiler.lookup.LocalTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeIds
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities
import org.eclipse.jdt.internal.core.util.Util
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.zaluum.nide.compiler._
import java.nio.charset.Charset
import java.io.ByteArrayInputStream
import org.objectweb.asm.Opcodes
import org.zaluum.nide.compiler.Reporter
import org.zaluum.nide.compiler.LocalScope
import org.zaluum.nide.compiler.Scope
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.ProblemReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.BinaryTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.eclipse.jdt.internal.compiler.ast.NormalAnnotation
import org.zaluum.annotation.Box
import org.eclipse.jdt.internal.compiler.ISourceElementRequestor
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.Platform
import org.eclipse.ui.PlatformUI
import org.eclipse.jdt.core.JavaCore
import org.eclipse.core.resources.ResourcesPlugin
import org.zaluum.nide.eclipse.EclipseUtils
import javax.swing.JPanel
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding

class ZaluumCompilationUnitDeclaration(
  problemReporter: ProblemReporter,
  compilationResult: CompilationResult,
  sourceLength: Int,
  sourceUnit: ICompilationUnit,
  compilerOptions: CompilerOptions)
  extends CompilationUnitDeclaration(problemReporter, compilationResult, sourceLength) {
  import ZaluumCompilationUnitDeclaration._

  var tree: BoxDef = _
  var a: Analyzer = _

  object JDTScope extends RootSymbol {
    def alreadyDefinedBoxType(name: Name): Boolean = false
    private def fail = throw new UnsupportedOperationException()
    var boxes = Map[Name, BoxTypeSymbol]()
    def lookupType(name: Name): Option[Type] = {
      zaluumScope.getJavaType(name)
    }
    def lookupBoxType(name: Name): Option[BoxType] = {
      boxes.get(name).orElse {
        zaluumScope.getBoxType(name)
      }
    }
    def enter(b: BoxTypeSymbol) {
      boxes += (b.name -> b)
    }
    def enter(p: PortSymbol) {}
    def enter(a: ValSymbol) {}
    def enter(p: ParamSymbol) {}
    def compilationUnitScope = zaluumScope
  }

  override def buildCompilationUnitScope(lookupEnvironment: LookupEnvironment) = {
    new ZaluumCompilationUnitScope(this, lookupEnvironment)
  }
  override def getSpecialDomCompilationUnit(ast: org.eclipse.jdt.core.dom.AST): org.eclipse.jdt.core.dom.CompilationUnit = {
    new ZaluumDomCompilationUnit(ast, tree)
  }
  def zaluumScope = scope.asInstanceOf[ZaluumCompilationUnitScope]

  def fileMainName = toMainName(compilationResult.getFileName).mkString
  import JDTInternalUtils._

  def fqName = aToString(currentPackage.getImportName) + tree.name.str
  private def createLineSeparator() = {
    // one char per line
    val treeSize = tree.children.size + 1
    compilationResult.lineSeparatorPositions = Array.range(1, (treeSize * 2) + 1, 2)
  }
  def populateCompilationUnitDeclaration() {
    try {
      sourceUnit match {
        case p: PreParsedZaluumCompilationUnit ⇒
          tree = p.tree.asInstanceOf[BoxDef]
        case _ ⇒
          val contents = new String(sourceUnit.getContents)
          tree = Parser.readTree(contents, Name(fileMainName))
      }
      createLineSeparator()
      val reporter = new Reporter() {
        override def report(str: String, mark: Option[Int] = None) {
          super.report(str, mark)
          ignoreFurtherInvestigation = true
          createProblem(str, mark.getOrElse(-1))
        }
      }
      val scope = JDTScope
      a = new Analyzer(reporter, tree, scope)
      a.runNamer()
      createPackageDeclaration()
      createTypeDeclarations()
    } catch { case e => e.printStackTrace }
  }

  def createProblem(msg: String, line: Int) {
    val p = new DefaultProblemFactory().createProblem(getFileName, 0, Array(msg), 0, Array(msg), ProblemSeverities.Error, 0, 1, line, 1)
    problemReporter.record(p, compilationResult, this)
  }
  def createPackageDeclaration() {
    val pkgArr = stringToA(tree.pkg.str)
    if (tree.pkg.str != "") {
      currentPackage = new ImportReference(pkgArr, Array.fill(pkgArr.length)(0), true, ClassFileConstants.AccDefault)
      currentPackage.declarationSourceStart = currentPackage.sourceStart
      currentPackage.declarationSourceEnd = currentPackage.sourceEnd
      currentPackage.declarationEnd = currentPackage.sourceEnd
    }
  }
  def createTypeDeclarations() {
    types = Array(createTypeDeclaration(tree, None))
  }
  def createTypeDeclaration(b: BoxDef, outer: Option[TypeDeclaration]): TypeDeclaration = {
    val typeDeclaration = new ZaluumTypeDeclaration(compilationResult, b)
    typeDeclaration.name = b.name.str.toCharArray
    outer match {
      case Some(o) ⇒
        typeDeclaration.bits |= ASTNode.IsMemberType
        typeDeclaration.modifiers = Opcodes.ACC_PUBLIC | Opcodes.ACC_STATIC
      case None ⇒
        typeDeclaration.modifiers = Opcodes.ACC_PUBLIC
        val annotation = new MarkerAnnotation(createTypeReference(Name(classOf[Box].getName), b), start(b))
        typeDeclaration.annotations = Array(annotation);

    }
    tree.superName foreach { n ⇒
      typeDeclaration.superclass = createTypeReference(n, b)
    }
    typeDeclaration.superInterfaces = Array();
    typeDeclaration.methods = createMethodAndConstructorDeclarations(b)
    typeDeclaration.fields = createFieldDeclarations(b)
    val children = for (
      t ← b.defs;
      val benc = t.asInstanceOf[BoxDef]
    ) yield {
      val encDec = createTypeDeclaration(benc, Some(typeDeclaration))
      encDec.enclosingType = typeDeclaration
      encDec
    }
    typeDeclaration.memberTypes = children.toArray
    typeDeclaration.declarationSourceEnd = end(b);
    typeDeclaration.declarationSourceStart = start(b);
    typeDeclaration.bodyStart = start(b);
    typeDeclaration.bodyEnd = end(b);
    typeDeclaration.sourceStart = start(b);
    typeDeclaration.sourceEnd = end(b);

    typeDeclaration
  }
  def start(t: Tree) = {
    (t.line - 1) * 2
  }
  def end(t: Tree) = {
    start(t) + 1
  }
  def createMethodAndConstructorDeclarations(b: BoxDef): Array[AbstractMethodDeclaration] = {
    val constructor = new ConstructorDeclaration(compilationResult)
    constructor.bits |= ASTNode.IsDefaultConstructor
    constructor.modifiers = ClassFileConstants.AccPublic
    constructor.selector = b.name.str.toCharArray

    val ref = createTypeReference(Name("void"), b)
    //val arg = new Argument("par".toCharArray, NON_EXISTENT_POSITION, ref, ClassFileConstants.AccPublic)
    val meth = new MethodDeclaration(compilationResult)
    meth.modifiers = ClassFileConstants.AccPublic
    meth.selector = "apply".toCharArray
    //meth.arguments = Array(arg)
    meth.returnType = createTypeReference(Name("void"), b)
    meth.thrownExceptions = null
    meth.sourceStart = start(b)
    meth.sourceEnd = end(b)
    meth.bodyStart = start(b)
    meth.bodyEnd = end(b)
    meth.declarationSourceStart = start(b)
    meth.declarationSourceEnd = end(b)
    Array(constructor, meth)
  }
  def createFieldDeclarations(b: BoxDef): Array[FieldDeclaration] = {
    val res = Buffer[FieldDeclaration]()
    //vals
    val bs = b.sym
    for (vs ← bs.valsAlphabeticOrder; if (vs.tpe.isInstanceOf[BoxTypeSymbol])) {
      val f = new FieldDeclaration(vs.name.str.toCharArray, start(vs.decl), end(vs.decl))
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(vs.tpe.name, vs.decl)
      res += f
    }
    //ports
    for (t ← b.ports; val p = t.asInstanceOf[PortDef]) {
      val f = new FieldDeclaration(p.name.str.toCharArray, start(p), end(p))
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(p.typeName, p)
      val cl = p.dir match {
        case Out ⇒ classOf[org.zaluum.annotation.Out].getName
        case _ ⇒ classOf[org.zaluum.annotation.In].getName
      }
      val annotation = new MarkerAnnotation(createTypeReference(Name(cl), p), start(p))
      f.annotations = Array(annotation)
      res += f
    }
    //widget
    {
      val f = new FieldDeclaration("_widget".toCharArray, 0, 1) // really ugly
      f.modifiersSourceStart = 0
      f.declarationEnd = 1
      f.endPart1Position = 1
      f.endPart2Position = 1
      f.declarationSourceStart = 0
      f.declarationSourceEnd = 1
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(Name(classOf[JPanel].getName), b)
      res += f
    }
    res.toArray
  }
  def createTypeReference(name: Name, t: Tree): TypeReference = {
    val tpe =
      name.asArray match {
        case Some((leaf, dim)) ⇒
          new ArrayTypeReference(leaf.str.toCharArray, dim, NON_EXISTENT_POSITION);
        case None ⇒
          if (nameToPrimitiveTypeId.contains(name.str)) {
            TypeReference.baseTypeReference(nameToPrimitiveTypeId(name.str), 0)
          } else if (!name.str.contains('.')) {
            new SingleTypeReference(name.str.toCharArray, NON_EXISTENT_POSITION)
          } else {
            val compoundName = CharOperation.splitOn('.', name.str.toCharArray)
            new QualifiedTypeReference(compoundName, Array.fill(compoundName.length)(NON_EXISTENT_POSITION))
          }
      }
    tpe.sourceStart = start(t)
    tpe.sourceEnd = end(t)
    tpe
  }

  override def generateCode() {
    def generate(tpe: ZaluumTypeDeclaration, enclosing: Option[ZaluumTypeDeclaration]) {
      val binding: SourceTypeBinding = tpe.binding
      if (binding != null) {
        val boxDef = tpe.b
        val classTree = new TreeToClass(boxDef, a.global).run()
        val name = binding.constantPoolName()
        compilationResult.record(name,
          new ZaluumClassFile(name.mkString, ByteCodeGen.dump(classTree), binding, name.mkString.replace('.', '/')))
        for (child ← tpe.memberTypes) generate(child.asInstanceOf[ZaluumTypeDeclaration], Some(tpe));
      }
    }
    if (!ignoreFurtherInvestigation && !ignoreMethodBodies) {
      try {
        generate(types(0).asInstanceOf[ZaluumTypeDeclaration], None)
      } catch { case e => e.printStackTrace }
    }
  }
  override def resolve() {
    super.resolve() // FIXME run or not?
    try {
      a.runResolve(types(0).asInstanceOf[ZaluumTypeDeclaration], this)
      a.runCheck()
      checkZaluumLibraryPresent()
    } catch { case e => e.printStackTrace }
  }
  def checkZaluumLibraryPresent() {
    // add a descriptive error to help users
    val errors = compilationResult.getErrors
    if (errors != null && errors.exists(_.getMessage == "org.zaluum cannot be resolved to a type")) {
      createProblem("Zaluum Runtime library is not in the classpath. Add org.zaluum.runtime jar to fix this problem.", 1)
    }
  }
  override def analyseCode() {
  }
  override def abort(abortLevel: Int, problem: CategorizedProblem) {
    super.abort(abortLevel, problem)
  }
}

// HELPERS
object ZaluumCompilationUnitDeclaration {
  def nameToPrimitiveTypeId = Map(
    "double" -> TypeIds.T_double,
    "int" -> TypeIds.T_int,
    "float" -> TypeIds.T_float,
    "long" -> TypeIds.T_long,
    "boolean" -> TypeIds.T_boolean,
    "byte" -> TypeIds.T_byte,
    "char" -> TypeIds.T_char,
    "short" -> TypeIds.T_short,
    "void" -> TypeIds.T_void)
  def positionsFor(reference: Array[Array[Char]], start: Long, end: Long): Array[Long] = {
    val result = Array.ofDim[Long](reference.length);
    val max = result.length
    if (start < end) {
      // Do the right thing
      var pos = start;
      for (i ← 0 until max) {
        val s = pos;
        pos = pos + reference(i).length - 1; // jump to the last char of the name
        result(i) = ((s << 32) | pos);
        pos += 2; // jump onto the following '.' then off it
      }
    } else {
      // FIXASC this case shouldn't happen (end<start) - uncomment following if to collect diagnostics
      var pos = (start << 32) | start;
      for (i ← 0 until max) {
        result(i) = pos;
      }
    }
    return result;
  }
  def toMainName(fileName: Array[Char]): Array[Char] = {
    if (fileName == null) {
      return Array.ofDim(0);
    }
    var start = CharOperation.lastIndexOf('/', fileName) + 1;
    if (start == 0 || start < CharOperation.lastIndexOf('\\', fileName))
      start = CharOperation.lastIndexOf('\\', fileName) + 1;

    var end = CharOperation.lastIndexOf('.', fileName);
    if (end == -1)
      end = fileName.length;

    return CharOperation.subarray(fileName, start, end);
  }
  lazy val NON_EXISTENT_POSITION = toPos(-1, -2)
  def toPos(start: Long, end: Long): Long = {
    if (start == 0 && end <= 0) {
      return NON_EXISTENT_POSITION;
    }
    return ((start << 32) | end);
  }
}