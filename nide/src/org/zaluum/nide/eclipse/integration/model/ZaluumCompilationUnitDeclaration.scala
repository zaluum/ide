package org.zaluum.nide.eclipse.integration.model

import scala.collection.mutable.Buffer
import org.eclipse.jdt.core.compiler.CategorizedProblem
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration
import org.eclipse.jdt.internal.compiler.ast.ImportReference
import org.eclipse.jdt.internal.compiler.ast.MarkerAnnotation
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.ast.TypeReference
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeIds
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.objectweb.asm._
import org.zaluum.annotation.Box
import org.zaluum.nide.compiler.Analyzer
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.ByteCodeGen
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.Parser
import org.zaluum.nide.compiler.Reporter
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.TreeToClass
import JDTInternalUtils.aToString
import JDTInternalUtils.stringToA
import ZaluumCompilationUnitDeclaration.NON_EXISTENT_POSITION
import ZaluumCompilationUnitDeclaration.nameToPrimitiveTypeId
import ZaluumCompilationUnitDeclaration.toMainName
import javax.swing.JPanel
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference
import org.zaluum.nide.compiler.In
import org.eclipse.jdt.internal.compiler.ast.Argument
import org.zaluum.annotation.Apply
import org.eclipse.jdt.internal.compiler.ast.NormalAnnotation
import org.eclipse.jdt.internal.compiler.ast.MemberValuePair
import org.eclipse.jdt.internal.compiler.ast.ArrayInitializer
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.impl.StringConstant
import org.eclipse.jdt.internal.compiler.ast.StringLiteral

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
      val scope = zaluumScope
      a = new Analyzer(reporter, tree)
      a.runNamer()
      createPackageDeclaration()
      createTypeDeclarations()
    } catch { case e ⇒ e.printStackTrace }
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
    typeDeclaration.superInterfaces = Array();
    typeDeclaration.methods = createMethodAndConstructorDeclarations(b)
    typeDeclaration.fields = createFieldDeclarations(b)
    typeDeclaration.memberTypes = Array()
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
    // TODO sorted methods accsortedmethods?
    val meth = new MethodDeclaration(compilationResult)
    b.template.ports find { _.dir == Out } match {
      case Some(p) ⇒
        meth.selector = p.name.str.toCharArray
        meth.returnType = createTypeReference(p.typeName, b)
      case None ⇒
        meth.selector = TreeToClass.defaultMethodName.toCharArray
        meth.returnType = createTypeReference(Name("void"), b)
    }
    val ins = b.template.ports filter { _.dir == In } sortBy { _.name.str }
    meth.arguments = ins map { p ⇒
      val ref = createTypeReference(p.typeName, p)
      val a = new Argument(p.name.str.toCharArray, compressPos(start(b), end(b)), ref, 0)
      a.declarationSourceStart = start(b);
      a
    } toArray;
    if (meth.arguments.size == 0) meth.arguments = null;
    val annotation = new NormalAnnotation(createTypeReference(Name(classOf[Apply].getName), b), start(b))
    val expr = new ArrayInitializer
    expr.expressions = ins.map { i ⇒ new StringLiteral(i.name.str.toCharArray, start(b), end(b), b.line) }.toArray
    annotation.memberValuePairs = Array(new MemberValuePair("paramNames".toCharArray, start(b), end(b), expr))
    meth.annotations = Array(annotation)
    meth.modifiers = ClassFileConstants.AccPublic
    meth.thrownExceptions = null
    meth.sourceStart = start(b)
    meth.sourceEnd = end(b)
    meth.bodyStart = start(b)
    meth.bodyEnd = end(b)
    meth.declarationSourceStart = start(b)
    meth.declarationSourceEnd = end(b)
    Array(constructor, meth)
  }
  // tpe not yet initialized
  def createFieldDeclarations(b: BoxDef): Array[FieldDeclaration] = {
    val res = Buffer[FieldDeclaration]()
    //vals 
    for (block ← b.template.blocks; v ← block.valDefs) {
      val f = new FieldDeclaration(v.sym.fqName.str.toCharArray, start(v), end(v))
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(v.typeName, v)
      res += f
    }
    //fields
    for (p ← b.template.ports filter { _.dir == Out } drop (1)) {
      val f = new FieldDeclaration(p.name.str.toCharArray, start(p), end(p))
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(p.typeName, p)
      val cl = classOf[org.zaluum.annotation.Out].getName
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
    val pos = toPos(start(t), end(t))
    val tpe =
      name.asArray match {
        case Some((leaf, dim)) ⇒
          if (nameToPrimitiveTypeId.contains(leaf.str)) {
            val br = TypeReference.baseTypeReference(nameToPrimitiveTypeId(leaf.str), dim)
            br.sourceStart = -1
            br.sourceEnd = -2
            br
          } else if (leaf.str.contains('.')) {
            val compoundName = CharOperation.splitOn('.', leaf.str.toCharArray)
            new ArrayQualifiedTypeReference(compoundName, dim, Array.fill(compoundName.length)(pos))
          } else
            new ArrayTypeReference(leaf.str.toCharArray, dim, pos);
        case None ⇒
          if (nameToPrimitiveTypeId.contains(name.str)) {
            val br = TypeReference.baseTypeReference(nameToPrimitiveTypeId(name.str), 0)
            br.sourceStart = -1
            br.sourceEnd = -2
            br
          } else if (!name.str.contains('.')) {
            new SingleTypeReference(name.str.toCharArray, pos)
          } else {
            val compoundName = CharOperation.splitOn('.', name.str.toCharArray)
            new QualifiedTypeReference(compoundName, Array.fill(compoundName.length)(pos))
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
          val classTree = new TreeToClass(boxDef, zaluumScope /*should be per class*/ , this.zaluumScope).run()
          val name = binding.constantPoolName()
          compilationResult.record(name,
            new ZaluumClassFile(name.mkString, ByteCodeGen.dump(classTree), binding, name.mkString.replace('.', '/')))
          for (child ← tpe.memberTypes) generate(child.asInstanceOf[ZaluumTypeDeclaration], Some(tpe));
        }
      }
    if (!ignoreFurtherInvestigation && !ignoreMethodBodies && a.reporter.errors.isEmpty) {
      try {
        generate(types(0).asInstanceOf[ZaluumTypeDeclaration], None)
      } catch { case e ⇒ e.printStackTrace }
    }
  }
  override def resolve() {
    super.resolve()
    try {
      val ztd = types(0).asInstanceOf[ZaluumTypeDeclaration]
      val scope = ztd.scope.asInstanceOf[ZaluumClassScope]
      tree.sym.javaScope = scope //  a bit ugly...
      a.runResolve(this, zaluumScope)
      a.runCheck()
      checkZaluumLibraryPresent()
    } catch { case e ⇒ e.printStackTrace }
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
  def compressPos(start: Int, end: Int) = (start << 32) | end;
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