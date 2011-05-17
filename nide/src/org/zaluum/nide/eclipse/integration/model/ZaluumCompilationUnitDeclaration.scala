package org.zaluum.nide.eclipse.integration.model

import scala.collection.mutable.Buffer
import org.eclipse.jdt.core.compiler.CategorizedProblem;
import org.eclipse.jdt.core.compiler.CharOperation;
import org.eclipse.jdt.internal.compiler.ast.SingleMemberAnnotation;
import org.eclipse.jdt.internal.compiler.ast.MarkerAnnotation;
import org.eclipse.jdt.internal.compiler.ASTVisitor;
import org.eclipse.jdt.internal.compiler.CompilationResult;
import org.eclipse.jdt.internal.compiler.ast.ASTNode;
import org.eclipse.jdt.internal.compiler.ast.AbstractMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.Annotation;
import org.eclipse.jdt.internal.compiler.ast.AnnotationMethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.Argument;
import org.eclipse.jdt.internal.compiler.ast.ArrayInitializer;
import org.eclipse.jdt.internal.compiler.ast.ArrayQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ArrayTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ClassLiteralAccess;
import org.eclipse.jdt.internal.compiler.ast.CompilationUnitDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ConstructorDeclaration;
import org.eclipse.jdt.internal.compiler.ast.FieldDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ImportReference;
import org.eclipse.jdt.internal.compiler.ast.Javadoc;
import org.eclipse.jdt.internal.compiler.ast.MethodDeclaration;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedQualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.ParameterizedSingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.QualifiedTypeReference;
import org.eclipse.jdt.internal.compiler.ast.SingleTypeReference;
import org.eclipse.jdt.internal.compiler.ast.StringLiteral;
import org.eclipse.jdt.internal.compiler.ast.TypeDeclaration;
import org.eclipse.jdt.internal.compiler.ast.TypeParameter;
import org.eclipse.jdt.internal.compiler.ast.TypeReference;
import org.eclipse.jdt.internal.compiler.ast.Wildcard;
import org.eclipse.jdt.internal.compiler.classfmt.ClassFileConstants;
import org.eclipse.jdt.internal.compiler.impl.CompilerOptions;
import org.eclipse.jdt.internal.compiler.impl.IrritantSet;
import org.eclipse.jdt.internal.compiler.lookup.BlockScope;
import org.eclipse.jdt.internal.compiler.lookup.CompilationUnitScope;
import org.eclipse.jdt.internal.compiler.lookup.LocalTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.LookupEnvironment;
import org.eclipse.jdt.internal.compiler.lookup.SourceTypeBinding;
import org.eclipse.jdt.internal.compiler.lookup.TypeIds;
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation;
import org.eclipse.jdt.internal.compiler.problem.DefaultProblemFactory;
import org.eclipse.jdt.internal.compiler.problem.ProblemReporter;
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities;
import org.eclipse.jdt.internal.core.util.Util;
import org.objectweb.asm.Opcodes;

class ZaluumCompilationUnitDeclaration(
   problemReporter : ProblemReporter , 
   compilationResult : CompilationResult,
   sourceLength : Int,
   /*groovyCompilationUnit : CompilationUnit ,groovySourceUnit : SourceUnit , */compilerOptions : CompilerOptions )
   extends CompilationUnitDeclaration(problemReporter, compilationResult, sourceLength){
  import ZaluumCompilationUnitDeclaration._
  def processToPhase(phase:Int) = true
  def populateCompilationUnitDeclaration() {
    createPackageDeclaration()
    createTypeDeclarations()
  }
  def createPackageDeclaration(){
    val packageReference =  CharOperation.splitOn('.',"org.zaluum.example".toCharArray)
    currentPackage = new ImportReference(packageReference,positionsFor(packageReference,0,1),true, ClassFileConstants.AccDefault)
    currentPackage.declarationSourceStart = currentPackage.sourceStart
    currentPackage.declarationSourceEnd = currentPackage.sourceEnd
    currentPackage.declarationEnd = currentPackage.sourceEnd
  }
  def mainNameArr = toMainName(compilationResult.getFileName)
  def createTypeDeclarations(){
    val typeDeclarations = Buffer[TypeDeclaration]()
    println("mainName " + mainNameArr.mkString )
    val typeDeclaration = new ZaluumTypeDeclaration(compilationResult)
    typeDeclaration.name = mainNameArr
    typeDeclaration.modifiers = Opcodes.ACC_PUBLIC
    configureSuperClass(typeDeclaration)
    configureSuperInterfaces(typeDeclaration)
    typeDeclaration.methods = createMethodAndConstructorDeclarations()
    typeDeclaration.fields = createFieldDeclarations()
    typeDeclarations += typeDeclaration
    
    types = typeDeclarations.toArray
  }
  def createMethodAndConstructorDeclarations() : Array[AbstractMethodDeclaration] = {
    val constructor = new ConstructorDeclaration(compilationResult)
    constructor.bits |= ASTNode.IsDefaultConstructor
    constructor.modifiers = ClassFileConstants.AccPublic
    constructor.selector = "constructor".toCharArray
    val ref = createTypeReference("int")
    val arg =  new Argument("par".toCharArray, NON_EXISTENT_POSITION,  ref, ClassFileConstants.AccPublic)
    val meth =  new MethodDeclaration(compilationResult)
    meth.modifiers = ClassFileConstants.AccPublic
    meth.selector = "meth".toCharArray
    meth.arguments = Array(arg)
    meth.returnType = createTypeReference("void")
    meth.thrownExceptions = null
    
    Array(constructor,meth)
  }
  def createTypeReference(name:String) : TypeReference = {
    // array todo
    if (nameToPrimitiveTypeId.contains(name)){
      TypeReference.baseTypeReference(nameToPrimitiveTypeId(name),0)
    }else if (!name.contains('.')) {
      new SingleTypeReference(name.toCharArray, NON_EXISTENT_POSITION)      
    }else {
      val compoundName = CharOperation.splitOn('.',name.toCharArray)
      new QualifiedTypeReference(compoundName, Array.fill(compoundName.length)(NON_EXISTENT_POSITION))
    }
  }
  def createFieldDeclarations() : Array[FieldDeclaration] =  Array()
  def configureSuperInterfaces(tpe : TypeDeclaration) {}
  def configureSuperClass(tpe:TypeDeclaration) {}
  def className = "org.zaluum.example." + mainNameArr.mkString
  def path = className.replace('.','/')
  override def generateCode() {
    println("generate code" + className)
    if (processToPhase(2)){
      val classbytes = Array[Byte]()
      val binding : SourceTypeBinding= this.types(0).binding
      println("binding = " + binding)
      compilationResult.record(className.toCharArray, 
          new ZaluumClassFile(className, simpleCode, binding,path))
    }
  }
  def simpleCode =  {
    import org.objectweb.asm._
    import Opcodes._
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, path, null, null , null);
    {
      val mv = cw.visitMethod(ACC_PUBLIC, "meth", "(I)V", null,null)
      mv.visitCode()
      val l0 = new Label();
      mv.visitLabel(l0);
      
      mv.visitInsn(RETURN);
      val lend = new Label();
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", "L"+path+";", null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    {
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null,null)
      mv.visitCode()
      val l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
      mv.visitInsn(RETURN);
      val lend = new Label();
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", "L"+path+";", null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    cw.visitEnd()
    cw.toByteArray
    
  }
  override def resolve() {
    
  }
  override def analyseCode() {
    
  }
  override def abort(abortLevel: Int, problem:CategorizedProblem) {
    super.abort(abortLevel,problem)
  }
} 




// HELPERS
object ZaluumCompilationUnitDeclaration {
  def nameToPrimitiveTypeId = Map(
      "double"-> TypeIds.T_double,
    "int"-> TypeIds.T_int,
    "float"-> TypeIds.T_float,
    "long"-> TypeIds.T_long,
    "boolean"-> TypeIds.T_boolean,
    "byte"-> TypeIds.T_byte,
    "char"-> TypeIds.T_char,
    "short"-> TypeIds.T_short,
    "void"-> TypeIds.T_void
  )
  def positionsFor(reference : Array[Array[Char]], start : Long , end: Long) : Array[Long] = {
    val result = Array.ofDim[Long](reference.length);
    val max = result.length
    if (start < end) {
      // Do the right thing
      var pos = start;
      for (i <- 0 until max) {
        val s = pos;
        pos = pos + reference(i).length - 1; // jump to the last char of the name
        result(i) = ((s << 32) | pos);
        pos += 2; // jump onto the following '.' then off it
      }
    } else {
      // FIXASC this case shouldn't happen (end<start) - uncomment following if to collect diagnostics
      var pos = (start << 32) | start;
      for (i <- 0 until max) {
        result(i) = pos;
      }
    }
    return result;
  }
  def toMainName(fileName:Array[Char]) : Array[Char] = {
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
  lazy val NON_EXISTENT_POSITION = toPos(-1,-2)
  def toPos(start : Long, end : Long) : Long = {
    if (start == 0 && end <= 0) {
      return NON_EXISTENT_POSITION;
    }
    return ((start << 32) | end);
  }
}