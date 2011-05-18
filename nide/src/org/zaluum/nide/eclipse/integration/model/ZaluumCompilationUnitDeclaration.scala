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
import org.zaluum.nide.eclipse.ZaluumBuilder
import java.nio.charset.Charset
import java.io.ByteArrayInputStream
import org.objectweb.asm.Opcodes
import org.zaluum.nide.compiler.Reporter
import org.zaluum.nide.compiler.LocalScope
import org.zaluum.nide.compiler.Scope
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.ProblemReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.BinaryTypeBinding

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
  /*def processToPhase(phase:Int) = {
    println("process to phase " + phase)
    phase match {
      case 0 =>
        val contents = new String(sourceUnit.getContents)
        val byteContents = contents.getBytes(Charset.forName("ISO-8859-1")) // TODO ??    
        tree = ZaluumBuilder.readTree(new ByteArrayInputStream(byteContents), Name(mainNameArr.mkString))
        println(tree)
      case _ =>
        
    }
    true
  }*/
  class JDTScope extends Scope {
    def alreadyDefinedBoxType(name: Name): Boolean = false
    private def fail = throw new UnsupportedOperationException()
    def lookupPort(name: Name): Option[Symbol] = fail
    def lookupVal(name: Name): Option[Symbol] = fail
    def lookupType(name: Name): Option[Type] = {
      val tpe = scope.getType(name.str.toArray)
      if (tpe != null) {
        tpe match {
          case r: ReferenceBinding ⇒ 
            val nme= r.compoundName.mkString
            Some(new JavaType(root, Name(nme)))
            
          case _ ⇒ None
        }
      } else None
    }
    def lookupBoxType(name: Name): Option[Type] = {
      val compoundname = name.str.split('.').map{_.toCharArray}
      val tpe = scope.getType(compoundname,compoundname.length)
      if (tpe!=null) {
        tpe match {
          //case z: ZaluumTypeDeclaration => None
          case p : ProblemReferenceBinding => 
            println(p.toString)
            None
          case r : ReferenceBinding =>
            println("store annotations = "+ compilerOptions.storeAnnotations)
            println("annotations " + r.compoundName.map(_.mkString).mkString(".") + r.getAnnotations.length)
            println("methods " + r.getClass + " " + r.methods().length)
            println("r " + r )
            r.getAnnotations foreach { a => 
              val atpe = a.getAnnotationType
              println ("atpe " + atpe.compoundName.mkString)
              false
            }
            None
          case _ => None
        }
      }else None
    }
    def lookupBoxTypeLocal(name: Name): Option[Type] = fail
    def enter[S <: Symbol](sym: S): S = {
      sym
    }
    val root: Symbol = new Symbol {
      def owner = this
      scope = JDTScope.this
      def name = Name("")
    }
  }
  def populateCompilationUnitDeclaration() {
    val contents = new String(sourceUnit.getContents)
    val byteContents = contents.getBytes(Charset.forName("ISO-8859-1")) // TODO ??    
    tree = ZaluumBuilder.readTree(new ByteArrayInputStream(byteContents), Name(mainNameArr.mkString))
    val reporter = new Reporter() {
      override def report(str:String, mark:Option[Location] = None ) {
        super.report(str,mark)
        createProblem(str)
      }
    }
    val scope = new JDTScope    
    a = new Analyzer(reporter, tree, scope)
    a.runNamer()
    println("filename = " + getFileName.mkString)
    //createProblem("taat")
    createPackageDeclaration()
    createTypeDeclarations()
  }
  def createProblem (msg:String) {
    val p = new DefaultProblemFactory().createProblem(getFileName, 0, Array(msg), 0, Array(msg), ProblemSeverities.Error, 0, 1, 1, 1)
    problemReporter.record(p, compilationResult, this)
  }
  def createPackageDeclaration() {
    val pkg = sourceUnit.getPackageName
    currentPackage = new ImportReference(pkg, positionsFor(pkg, 0, 1), true, ClassFileConstants.AccDefault)
    currentPackage.declarationSourceStart = currentPackage.sourceStart
    currentPackage.declarationSourceEnd = currentPackage.sourceEnd
    currentPackage.declarationEnd = currentPackage.sourceEnd
  }
  def mainNameArr = toMainName(compilationResult.getFileName)
  def createTypeDeclarations() {
    val typeDeclarations = Buffer[TypeDeclaration]()
    typeDeclarations += createTypeDeclaration(tree)
    types = typeDeclarations.toArray
  }
  def createTypeDeclaration(b:BoxDef) = {
    val typeDeclaration = new ZaluumTypeDeclaration(compilationResult)
    typeDeclaration.name = mainNameArr
    typeDeclaration.modifiers = Opcodes.ACC_PUBLIC
    tree.superName foreach { n =>
      typeDeclaration.superclass = createTypeReference(n.str)
    }
    typeDeclaration.methods = createMethodAndConstructorDeclarations(b)
    typeDeclaration.fields = createFieldDeclarations(b)
    typeDeclaration
  }
  def createMethodAndConstructorDeclarations(b:BoxDef): Array[AbstractMethodDeclaration] = {
    val constructor = new ConstructorDeclaration(compilationResult)
    constructor.bits |= ASTNode.IsDefaultConstructor
    constructor.modifiers = ClassFileConstants.AccPublic
    constructor.selector = b.name.str.toCharArray
    
    val ref = createTypeReference("void")
    //val arg = new Argument("par".toCharArray, NON_EXISTENT_POSITION, ref, ClassFileConstants.AccPublic)
    val meth = new MethodDeclaration(compilationResult)
    meth.modifiers = ClassFileConstants.AccPublic
    meth.selector = "apply".toCharArray
    //meth.arguments = Array(arg)
    meth.returnType = createTypeReference("void")
    meth.thrownExceptions = null

    Array(constructor, meth)
  }
  def createFieldDeclarations(b:BoxDef): Array[FieldDeclaration] = {
    val res = Buffer[FieldDeclaration]()
    for (t<-b.vals; val v = t.asInstanceOf[ValDef] ){
      val f = new FieldDeclaration(v.name.str.toCharArray,0,0)
      f.modifiers = Opcodes.ACC_PUBLIC
      f.`type` = createTypeReference(v.typeName.str)
      res += f
    }
    res.toArray
  }
  def createTypeReference(name: String): TypeReference = {
    // array todo
    if (nameToPrimitiveTypeId.contains(name)) {
      TypeReference.baseTypeReference(nameToPrimitiveTypeId(name), 0)
    } else if (!name.contains('.')) {
      new SingleTypeReference(name.toCharArray, NON_EXISTENT_POSITION)
    } else {
      val compoundName = CharOperation.splitOn('.', name.toCharArray)
      new QualifiedTypeReference(compoundName, Array.fill(compoundName.length)(NON_EXISTENT_POSITION))
    }
  }
  
  def className = "org.zaluum.example." + mainNameArr.mkString
  def path = className.replace('.', '/')
  override def generateCode() {
    println("generate code" + className)
    val classbytes = Array[Byte]()
    val binding: SourceTypeBinding = this.types(0).binding
    println("binding = " + binding)
    compilationResult.record(className.toCharArray,
      new ZaluumClassFile(className, simpleCode, binding, path))
  }
  def simpleCode = {
    import org.objectweb.asm._
    import Opcodes._
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, path, null, null, null);
    {
      val mv = cw.visitMethod(ACC_PUBLIC, "meth", "(I)V", null, null)
      mv.visitCode()
      val l0 = new Label();
      mv.visitLabel(l0);

      mv.visitInsn(RETURN);
      val lend = new Label();
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", "L" + path + ";", null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    {
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null)
      mv.visitCode()
      val l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
      mv.visitInsn(RETURN);
      val lend = new Label();
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", "L" + path + ";", null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    cw.visitEnd()
    cw.toByteArray

  }
  override def resolve() {
    println("resolve " + this)
    a.runResolve()
  }
  override def analyseCode() {
    println("analyse " + this)
    a.runCheck()
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