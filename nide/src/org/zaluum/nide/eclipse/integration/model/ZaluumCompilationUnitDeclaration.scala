package org.zaluum.nide.eclipse.integration.model

import scala.collection.mutable.Buffer
import org.eclipse.jdt.core.compiler.CharOperation
import org.eclipse.jdt.internal.compiler.ast.ASTNode
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.Argument
import org.eclipse.jdt.internal.compiler.ast.ArrayInitializer
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration
import org.eclipse.jdt.internal.compiler.ast.ImportReference
import org.eclipse.jdt.internal.compiler.ast.MarkerAnnotation
import org.eclipse.jdt.internal.compiler.ast.MemberValuePair
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration
import org.eclipse.jdt.internal.compiler.ast.NormalAnnotation
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference
import org.eclipse.jdt.internal.compiler.ast.StringLiteral
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration
import org.eclipse.jdt.internal.compiler.ast.TypeReference
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants
import org.eclipse.jdt.internal.compiler.env.ICompilationUnit
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeIds
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter
import org.eclipse.jdt.internal.compiler.ClassFile
import org.eclipse.jdt.internal.compiler.CompilationResult
import org.objectweb.asm.Opcodes
import org.zaluum.annotation.Apply
import org.zaluum.annotation.Box
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.ByteCodeGen
import org.zaluum.nide.compiler.Expressions
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.Parser
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.TreeToClass
import org.zaluum.nide.utils.JDTUtils.aToString
import org.zaluum.nide.utils.JDTUtils.stringToA
import ZaluumCompilationUnitDeclaration.compressPos
import ZaluumCompilationUnitDeclaration.nameToPrimitiveTypeId
import ZaluumCompilationUnitDeclaration.toMainName
import ZaluumCompilationUnitDeclaration.toPos
import org.zaluum.expr.BoxExpr
import org.zaluum.nide.compiler.BoxExprType
import org.zaluum.nide.compiler.Param
class ZaluumCompilationUnitDeclaration(
  problemReporter: ProblemReporter,
  compilationResult: CompilationResult,
  sourceLength: Int,
  sourceUnit: ICompilationUnit,
  compilerOptions: CompilerOptions)
    extends CompilationUnitDeclaration(problemReporter, compilationResult, sourceLength) {

  var tree: BoxDef = _
  override def buildCompilationUnitScope(lookupEnvironment: LookupEnvironment) = new ZaluumCompilationUnitScope(this, lookupEnvironment)
  override def getSpecialDomCompilationUnit(ast: org.eclipse.jdt.core.dom.AST): org.eclipse.jdt.core.dom.CompilationUnit = new ZaluumDomCompilationUnit(ast, tree)
  def zaluumScope = scope.asInstanceOf[ZaluumCompilationUnitScope]
  def fileMainName = toMainName(compilationResult.getFileName).mkString
  def fqName = aToString(currentPackage.getImportName) + tree.name.str
  private def createLineSeparator() = {
    // one char per line
    val treeSize = tree.deepchildren.size + 1
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
      createPackageDeclaration()
      createTypeDeclarations()
    } catch { case e ⇒ e.printStackTrace }
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
    typeDeclaration.superclass = createTypeReference(Name("javax.swing.JPanel"), b)
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
    val constructor = if (b.constructor.isEmpty) {
      val default = new ConstructorDeclaration(compilationResult)
      default.bits |= ASTNode.IsDefaultConstructor
      default.modifiers = ClassFileConstants.AccPublic
      default.selector = b.name.str.toCharArray
      default
    } else {
      val cons = new ConstructorDeclaration(compilationResult)
      cons.modifiers = ClassFileConstants.AccPublic
      cons.selector = b.name.str.toCharArray
      cons.arguments = b.constructor map { p ⇒
        val ref = createTypeReference(p.tpeName, b)
        val a = new Argument(p.name.str.toCharArray(), compressPos(start(b), end(b)), ref, 0)
        a.declarationSourceStart = start(b)
        a
      } toArray;
      cons
    }
    // TODO sorted methods accsortedmethods?
    val meth = new MethodDeclaration(compilationResult)
    b.template.ports find { _.dir == Out } match {
      case Some(p) ⇒
        meth.selector = p.name.str.toCharArray
        meth.returnType = createTypeReference(p.typeName, p)
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
  // !! tpe not yet initialized
  def createFieldDeclarations(b: BoxDef): Array[FieldDeclaration] = {
    val res = Buffer[FieldDeclaration]()
      //vals 
      def defineValsInBlock(block: Block) {
        for (v ← block.valDefs) {
          if (v.typeName == BoxExprType.fqName) {
            val f = new FieldDeclaration(v.name.str.toCharArray, start(v), end(v)) // keep synched with symbols.ValSymbol.fqName 
            f.modifiers = Opcodes.ACC_PUBLIC
            v.params.find(_.key == BoxExprType.typeSymbol.fqName).map { p ⇒ Name(p.valueStr) } match {
              case Some(name) ⇒ f.`type` = createTypeReference(name, v)
              case None       ⇒ f.`type` = createTypeReference(Name("java.lang.Object"), v)
            }
            res += f
          } else {
            for (t ← v.template; b ← t.blocks) {
              defineValsInBlock(b)
            }
          }
        }
      }
    defineValsInBlock(b.template.blocks.head)
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
    //constructor fields
    for (p ← b.constructor) {
      val f = new FieldDeclaration(p.name.str.toCharArray, start(b), end(b))
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(p.tpeName, b)
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
            TypeReference.baseTypeReference(nameToPrimitiveTypeId(leaf.str), dim)
          } else if (leaf.str.contains('.')) {
            val compoundName = CharOperation.splitOn('.', leaf.str.toCharArray)
            new ArrayQualifiedTypeReference(compoundName, dim, Array.fill(compoundName.length)(pos))
          } else
            new ArrayTypeReference(leaf.str.toCharArray, dim, pos);
        case None ⇒
          if (name.str == "") {
            TypeReference.baseTypeReference(TypeIds.T_void, 0)
          } else if (nameToPrimitiveTypeId.contains(name.str)) {
            TypeReference.baseTypeReference(nameToPrimitiveTypeId(name.str), 0)
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
      def generate(tpe: ZaluumTypeDeclaration) {
        val binding: SourceTypeBinding = tpe.binding
        if (binding != null) {
          val boxDef = tpe.b
          val classTree = new TreeToClass(boxDef, tpe.zaluumScope /*should be per class*/ , tpe.zaluumScope).run()
          val name = binding.constantPoolName()
          compilationResult.record(name,
            new ZaluumClassFile(
              name.mkString,
              ByteCodeGen.dump(classTree),
              binding,
              name.mkString.replace('.', '/')))

          for (inn ← classTree.inners) {
            val name = inn.fqName
            compilationResult.record(name.str.toCharArray(),
              new ZaluumClassFile(
                name.str,
                ByteCodeGen.dump(inn, classTree),
                binding,
                name.internal))
          }
          //for (child ← tpe.memberTypes) generate(child.asInstanceOf[ZaluumTypeDeclaration], Some(tpe));
        }
      }
    val ztd = types(0).asInstanceOf[ZaluumTypeDeclaration]
    if (!ignoreFurtherInvestigation && ztd.a.reporter.errors.isEmpty) {
      try {
        generate(ztd)
      } catch { case e ⇒ e.printStackTrace }
    } else {
      ClassFile.createProblemType(
        ztd,
        this.scope.referenceCompilationUnit().compilationResult);
    }
  }

  override def analyseCode() {
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