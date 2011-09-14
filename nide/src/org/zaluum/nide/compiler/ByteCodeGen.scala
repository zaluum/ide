package org.zaluum.nide.compiler

import org.objectweb.asm.{ Type ⇒ _, _ }
import Opcodes._
import org.zaluum.annotation.Box
object ByteCodeGen {
  var mv: MethodVisitor = null
  var cw: ClassWriter = null
  var thisName: Name = null
  def dump(bc: BoxClass): Array[Byte] = {
    thisName = bc.name
    cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, bc.name.internal, null, bc.superName.internal, null);
    for (inn ← bc.inners) {
      cw.visitInnerClass(inn.fqName.internal, bc.name.internal, inn.simpleName.str, 0)
    }
    cw.visitAnnotation(Name(classOf[Box].getName).descriptor, true).visitEnd
    bc.contents foreach { emit(_) }
    cw.visitEnd()
    cw.toByteArray
  }
  def dump(inn: RunnableClass, bc: BoxClass): Array[Byte] = {
    thisName = inn.fqName
    cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    cw.visit(V1_5, ACC_SUPER, inn.fqName.internal, null, "java/lang/Object", Array("java/lang/Runnable"));
    cw.visitInnerClass(inn.fqName.internal, bc.name.internal, inn.simpleName.str, 0);
    {
      val fv = cw.visitField(ACC_PUBLIC, TreeToClass.enclosingClassFieldName, bc.name.descriptor, null, null);
      fv.visitEnd();
    }
    {
      val mv = cw.visitMethod(0, "<init>", "(" + bc.name.descriptor + ")V", null, null);
      mv.visitCode();
      val l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitVarInsn(ALOAD, 1);
      mv.visitFieldInsn(PUTFIELD, inn.fqName.internal, TreeToClass.enclosingClassFieldName, bc.name.descriptor);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
      mv.visitInsn(RETURN);
      val l1 = new Label();
      mv.visitLabel(l1);
      mv.visitLocalVariable("this", inn.fqName.descriptor, null, l0, l1, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    emit(inn.run)
    mv.visitMaxs(-1, -1);
    cw.visitEnd()
    cw.toByteArray
  }
  def emitMethod(name: String, signature: String, tree: Tree, locals: List[(String, String, Int)], constructor: Option[Name], annotation: Option[List[Name]]) {
    mv = cw.visitMethod(ACC_PUBLIC, name, signature, null, null);
    annotation foreach { a ⇒
      val name = Name(classOf[org.zaluum.annotation.Apply].getName).descriptor
      val av = mv.visitAnnotation(name, true)
      val p = av.visitArray("paramNames")
      a.foreach(v ⇒ p.visit(null /*ok for array*/ , v.str))
      p.visitEnd()
      av.visitEnd()
    }
    mv.visitCode();
    constructor match {
      case Some(superName) ⇒
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, superName.internal, "<init>", "()V");
      case None ⇒
    }
    val l0 = new Label();
    mv.visitLabel(l0);
    tree.children foreach { emit(_) }
    val lend = new Label();
    mv.visitLabel(lend);
    mv.visitLocalVariable("this", thisName.descriptor, null, l0, lend, 0);
    for ((name, desc, i) ← locals) {
      mv.visitLocalVariable(name, desc, null, l0, lend, i);
    }
    mv.visitMaxs(-1, -1);
    mv.visitEnd();
  }
  def emit(tree: Tree): Unit = {
    tree match {
      case FieldDef(name, tpe, annotation, priv) ⇒
        // FIXME not really private field always public
        val f = cw.visitField(if (priv) ACC_PUBLIC else ACC_PUBLIC, name.str, tpe.descriptor, null, null)
        annotation foreach { name ⇒
          f.visitAnnotation(name.descriptor, true).visitEnd()
        }
        f.visitEnd
      case ConstructorMethod(c, superName) ⇒
        emitMethod("<init>", "()V", tree, List(), Some(superName), None)
      case Method(name, signature, stats, locals, paramsNames) ⇒
        emitMethod(name.str, signature, tree, locals, None, paramsNames)
      case New(typeName, param, signature) ⇒
        mv.visitTypeInsn(NEW, typeName.internal);
        mv.visitInsn(DUP);
        for (p ← param) emit(p)
        mv.visitMethodInsn(INVOKESPECIAL, typeName.internal, "<init>", signature);
      case NullConst ⇒
        mv.visitInsn(ACONST_NULL)
      case LocalRef(id, tpe) ⇒
        load(id, tpe)
      case Assign(lhs, rhs) ⇒
        lhs match {
          case LocalRef(id, tpe) ⇒
            emit(rhs)
            store(id, tpe)
          case Select(a, FieldRef(id, descriptor, fromClass)) ⇒
            emit(a)
            emit(rhs)
            mv.visitFieldInsn(PUTFIELD, fromClass.internal, id.str, descriptor)
          case FieldStaticRef(id, descriptor, fromClass) ⇒
            emit(rhs)
            mv.visitFieldInsn(PUTSTATIC, fromClass.internal, id.str, descriptor)
          case ArrayRef(index, arr, tpe) ⇒
            emit(arr)
            emit(index)
            emit(rhs)
            astore(tpe.fqName)
        }
      case While(body, cond) ⇒
        val start = new Label()
        mv.visitLabel(start)
        body foreach { emit }
        emit(cond)
        mv.visitJumpInsn(IFNE, start)
      case If(cond, trueBlock, falseBlock) ⇒
        val end = new Label()
        (trueBlock, falseBlock) match {
          case (Nil, Nil) ⇒
          case (tb, Nil) ⇒
            emit(cond)
            mv.visitJumpInsn(IFEQ, end)
            tb.foreach(emit)
            mv.visitLabel(end)
          case (Nil, fb) ⇒
            emit(cond)
            mv.visitJumpInsn(IFNE, end)
            fb.foreach(emit)
            mv.visitLabel(end)
          case (tb, fb) ⇒
            val elseLabel = new Label()
            emit(cond)
            mv.visitJumpInsn(IFEQ, elseLabel)
            tb.foreach(emit)
            mv.visitJumpInsn(GOTO, end)
            mv.visitLabel(elseLabel)
            fb.foreach(emit)
            mv.visitLabel(end)
        }
      case Select(a, b) ⇒
        emit(a)
        emit(b)
      case NewArray(sizes, tpe) ⇒
        sizes match {
          case i :: Nil ⇒
            emit(i)
            tpe match {
              case p: PrimitiveJavaType ⇒
                mv.visitIntInsn(NEWARRAY, asmType(p.fqName))
              case j: ClassJavaType ⇒
                mv.visitTypeInsn(ANEWARRAY, j.fqName.internal)
            }
          case more ⇒
            sizes foreach { i ⇒ emit(i) }
            val desc = "[" * sizes.length + tpe.fqName.descriptor
            mv.visitMultiANewArrayInsn(desc, sizes.length)
        }
      case ArrayRef(index, array, tpe) ⇒
        emit(array)
        emit(index)
        aload(tpe.fqName)
      case FieldRef(id, descriptor, fromClass) ⇒
        mv.visitFieldInsn(GETFIELD, fromClass.internal, id.str, descriptor)
      case FieldStaticRef(id, descriptor, fromClass) ⇒
        mv.visitFieldInsn(GETSTATIC, fromClass.internal, id.str, descriptor)
      case This ⇒
        mv.visitVarInsn(ALOAD, 0);
      case InvokeStatic(meth, params, fromClass, descriptor) ⇒
        params foreach { emit }
        mv.visitMethodInsn(INVOKESTATIC, fromClass.internal, meth, descriptor)
      case Invoke(obj, meth, param, fromClass, descriptor, interface) ⇒
        emit(obj)
        param foreach { emit(_) }
        val ins = if (interface) INVOKEINTERFACE else INVOKEVIRTUAL
        mv.visitMethodInsn(ins, fromClass.internal, meth, descriptor)
      case Pop ⇒
        mv.visitInsn(POP)
      case AStore(i: Int) ⇒
        mv.visitVarInsn(ASTORE, i)
      case ALoad(i: Int) ⇒
        mv.visitVarInsn(ALOAD, i)
      case Const(d, constTpe) ⇒
        emitConst(d, constTpe)
      case Return(t, tpe) ⇒
        emit(t)
        doReturn(tpe)
      case Return ⇒
        mv.visitInsn(RETURN)
      case True ⇒
        mv.visitInsn(ICONST_1)
      case Dup ⇒
        mv.visitInsn(DUP)
      case b: BinaryExpr ⇒
        emitBinaryExpr(b)
      case u: UnaryExpr ⇒
        emitUnaryExpr(u)

    }
  }
  def doReturn(t: Name) {
    import primitives._
    t match {
      case Boolean.fqName ⇒ mv.visitInsn(IRETURN)
      case Byte.fqName    ⇒ mv.visitInsn(IRETURN)
      case Char.fqName    ⇒ mv.visitInsn(IRETURN)
      case Short.fqName   ⇒ mv.visitInsn(IRETURN)
      case Int.fqName     ⇒ mv.visitInsn(IRETURN)
      case Long.fqName    ⇒ mv.visitInsn(LRETURN)
      case Float.fqName   ⇒ mv.visitInsn(FRETURN)
      case Double.fqName  ⇒ mv.visitInsn(DRETURN)
      case _              ⇒ mv.visitInsn(ARETURN)
    }
  }
  def emitConst(d: Any, constTpe: Name) {
    import primitives._
    if (d == null || d == 0 || d == false) {
      constTpe match {
        case Boolean.fqName ⇒ mv.visitInsn(ICONST_0)
        case Byte.fqName    ⇒ mv.visitInsn(ICONST_0)
        case Char.fqName    ⇒ mv.visitInsn(ICONST_0)
        case Short.fqName   ⇒ mv.visitInsn(ICONST_0)
        case Int.fqName     ⇒ mv.visitInsn(ICONST_0)
        case Long.fqName    ⇒ mv.visitInsn(LCONST_0)
        case Float.fqName   ⇒ mv.visitInsn(FCONST_0)
        case Double.fqName  ⇒ mv.visitInsn(DCONST_0)
        case _              ⇒ mv.visitInsn(ACONST_NULL)
      }
    } else {
      constTpe match {
        case Boolean.fqName ⇒ mv.visitInsn(if (d == true) ICONST_1 else ICONST_0)
        case Byte.fqName    ⇒ mv.visitLdcInsn(d.asInstanceOf[Byte].toInt) // TODO make clear how to cast
        case Char.fqName    ⇒ mv.visitLdcInsn(d.asInstanceOf[Char].toInt)
        case Short.fqName   ⇒ mv.visitLdcInsn(d.asInstanceOf[Short].toInt)
        case Int.fqName     ⇒ mv.visitLdcInsn(d.asInstanceOf[Int])
        case Long.fqName    ⇒ mv.visitLdcInsn(d.asInstanceOf[Long])
        case Float.fqName   ⇒ mv.visitLdcInsn(d.asInstanceOf[Float])
        case Double.fqName  ⇒ mv.visitLdcInsn(d.asInstanceOf[Double])
        case _              ⇒ mv.visitLdcInsn(d.asInstanceOf[String])
      }
    }
  }
  def asmType(t: Name) = t match {
    case primitives.Long.fqName    ⇒ T_LONG
    case primitives.Double.fqName  ⇒ T_DOUBLE
    case primitives.Float.fqName   ⇒ T_FLOAT
    case primitives.Int.fqName     ⇒ T_INT
    case primitives.Boolean.fqName ⇒ T_BOOLEAN
    case primitives.Byte.fqName    ⇒ T_BYTE
    case primitives.Char.fqName    ⇒ T_CHAR
    case primitives.Short.fqName   ⇒ T_SHORT
  }
  def aload(t: Name) = {
    t match {
      case primitives.Long.fqName    ⇒ mv.visitInsn(LALOAD)
      case primitives.Double.fqName  ⇒ mv.visitInsn(DALOAD)
      case primitives.Float.fqName   ⇒ mv.visitInsn(FALOAD)
      case primitives.Int.fqName     ⇒ mv.visitInsn(IALOAD)
      case primitives.Boolean.fqName ⇒ mv.visitInsn(BALOAD)
      case primitives.Byte.fqName    ⇒ mv.visitInsn(BALOAD)
      case primitives.Char.fqName    ⇒ mv.visitInsn(CALOAD)
      case primitives.Short.fqName   ⇒ mv.visitInsn(SALOAD)
      case _                         ⇒ mv.visitInsn(AALOAD)
    }
  }
  def astore(t: Name) = {
    t match {
      case primitives.Long.fqName    ⇒ mv.visitInsn(LASTORE)
      case primitives.Double.fqName  ⇒ mv.visitInsn(DASTORE)
      case primitives.Float.fqName   ⇒ mv.visitInsn(FASTORE)
      case primitives.Int.fqName     ⇒ mv.visitInsn(IASTORE)
      case primitives.Boolean.fqName ⇒ mv.visitInsn(BASTORE)
      case primitives.Byte.fqName    ⇒ mv.visitInsn(BASTORE)
      case primitives.Char.fqName    ⇒ mv.visitInsn(CASTORE)
      case primitives.Short.fqName   ⇒ mv.visitInsn(SASTORE)
      case _                         ⇒ mv.visitInsn(AASTORE)
    }
  }
  def store(id: Int, t: Name) = {
    t match {
      case primitives.Long.name    ⇒ mv.visitVarInsn(LSTORE, id)
      case primitives.Double.name  ⇒ mv.visitVarInsn(DSTORE, id)
      case primitives.Float.name   ⇒ mv.visitVarInsn(FSTORE, id)
      case primitives.Int.name     ⇒ mv.visitVarInsn(ISTORE, id)
      case primitives.Boolean.name ⇒ mv.visitVarInsn(ISTORE, id)
      case primitives.Byte.name    ⇒ mv.visitVarInsn(ISTORE, id)
      case primitives.Char.name    ⇒ mv.visitVarInsn(ISTORE, id)
      case primitives.Short.name   ⇒ mv.visitVarInsn(ISTORE, id)
      case _                       ⇒ mv.visitVarInsn(ASTORE, id)
    }
  }
  def load(id: Int, t: Name) = {
    t match {
      case primitives.Long.name    ⇒ mv.visitVarInsn(LLOAD, id)
      case primitives.Double.name  ⇒ mv.visitVarInsn(DLOAD, id)
      case primitives.Float.name   ⇒ mv.visitVarInsn(FLOAD, id)
      case primitives.Int.name     ⇒ mv.visitVarInsn(ILOAD, id)
      case primitives.Boolean.name ⇒ mv.visitVarInsn(ILOAD, id)
      case primitives.Byte.name    ⇒ mv.visitVarInsn(ILOAD, id)
      case primitives.Char.name    ⇒ mv.visitVarInsn(ILOAD, id)
      case primitives.Short.name   ⇒ mv.visitVarInsn(ILOAD, id)
      case _                       ⇒ mv.visitVarInsn(ALOAD, id)
    }
  }
  def emitBinaryExpr(e: BinaryExpr) = {
    emit(e.a)
    emit(e.b)
    import primitives._
    e match {
      case And(a, b, _)            ⇒ mv.visitInsn(IAND)
      case Or(a, b, _)             ⇒ mv.visitInsn(IOR)
      case Xor(a, b, _)            ⇒ mv.visitInsn(IXOR)

      case Add(a, b, Int)          ⇒ mv.visitInsn(IADD)
      case Add(a, b, Double)       ⇒ mv.visitInsn(DADD)
      case Add(a, b, Long)         ⇒ mv.visitInsn(LADD)
      case Add(a, b, Float)        ⇒ mv.visitInsn(FADD)

      case Sub(a, b, Int)          ⇒ mv.visitInsn(ISUB)
      case Sub(a, b, Double)       ⇒ mv.visitInsn(DSUB)
      case Sub(a, b, Long)         ⇒ mv.visitInsn(LSUB)
      case Sub(a, b, Float)        ⇒ mv.visitInsn(FSUB)

      case Mul(a, b, Int)          ⇒ mv.visitInsn(IMUL)
      case Mul(a, b, Double)       ⇒ mv.visitInsn(DMUL)
      case Mul(a, b, Long)         ⇒ mv.visitInsn(LMUL)
      case Mul(a, b, Float)        ⇒ mv.visitInsn(FMUL)

      case Div(a, b, Int)          ⇒ mv.visitInsn(IDIV)
      case Div(a, b, Double)       ⇒ mv.visitInsn(DDIV)
      case Div(a, b, Long)         ⇒ mv.visitInsn(LDIV)
      case Div(a, b, Float)        ⇒ mv.visitInsn(FDIV)

      case Rem(a, b, Int)          ⇒ mv.visitInsn(IREM)
      case Rem(a, b, Double)       ⇒ mv.visitInsn(DREM)
      case Rem(a, b, Long)         ⇒ mv.visitInsn(LREM)
      case Rem(a, b, Float)        ⇒ mv.visitInsn(FREM)

      case Lt(a, b, Int)           ⇒ emitIntCmp(IF_ICMPGE)
      case Le(a, b, Int)           ⇒ emitIntCmp(IF_ICMPGT)
      case Gt(a, b, Int)           ⇒ emitIntCmp(IF_ICMPLE)
      case Ge(a, b, Int)           ⇒ emitIntCmp(IF_ICMPLT)
      case Eq(a, b, Int)           ⇒ emitIntCmp(IF_ICMPNE)
      case Ne(a, b, Int)           ⇒ emitIntCmp(IF_ICMPEQ)

      case Eq(a, b, Boolean)       ⇒ emitIntCmp(IF_ICMPNE)
      case Ne(a, b, Boolean)       ⇒ emitIntCmp(IF_ICMPEQ)

      case ShiftLeft(a, b, Int)    ⇒ mv.visitInsn(ISHL)
      case ShiftRight(a, b, Int)   ⇒ mv.visitInsn(ISHR)
      case UShiftRight(a, b, Int)  ⇒ mv.visitInsn(IUSHR)
      case ShiftLeft(a, b, Long)   ⇒ mv.visitInsn(LSHL)
      case ShiftRight(a, b, Long)  ⇒ mv.visitInsn(LSHR)
      case UShiftRight(a, b, Long) ⇒ mv.visitInsn(LUSHR)

      case Lt(a, b, t)             ⇒ emitCmpl(t); emitIntCmp(IFGE)
      case Le(a, b, t)             ⇒ emitCmpl(t); emitIntCmp(IFGT)
      case Gt(a, b, t)             ⇒ emitCmpg(t); emitIntCmp(IFLE)
      case Ge(a, b, t)             ⇒ emitCmpg(t); emitIntCmp(IFLT)
      case Eq(a, b, t)             ⇒ emitCmpg(t); emitIntCmp(IFNE)
      case Ne(a, b, t)             ⇒ emitCmpg(t); emitIntCmp(IFEQ)

    }
  }
  def emitCmpg(t: PrimitiveJavaType) = t match {
    case primitives.Double ⇒ mv.visitInsn(DCMPG)
    case primitives.Float  ⇒ mv.visitInsn(FCMPG)
    case primitives.Long   ⇒ mv.visitInsn(LCMP)
  }
  def emitCmpl(t: PrimitiveJavaType) = t match {
    case primitives.Double ⇒ mv.visitInsn(DCMPL)
    case primitives.Float  ⇒ mv.visitInsn(FCMPL)
    case primitives.Long   ⇒ mv.visitInsn(LCMP)
  }
  def emitIntCmp(negatedCmp: Int) {
    val falseL = new Label()
    val end = new Label()
    mv.visitJumpInsn(negatedCmp, falseL)
    mv.visitInsn(ICONST_1)
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(falseL)
    mv.visitInsn(ICONST_0)
    mv.visitLabel(end)
  }
  def emitUnaryExpr(e: UnaryExpr) = {
    import primitives._
    emit(e.a)
    e match {
      case Not(a, Boolean)  ⇒ emitIntCmp(IFNE)
      case Not(a, Int)      ⇒ mv.visitInsn(ICONST_M1); mv.visitInsn(IXOR)
      case Minus(a, Int)    ⇒ mv.visitInsn(INEG)
      case Minus(a, Long)   ⇒ mv.visitInsn(LNEG)
      case Minus(a, Float)  ⇒ mv.visitInsn(FNEG)
      case Minus(a, Double) ⇒ mv.visitInsn(DNEG)
      case I2B(_)           ⇒ mv.visitInsn(Opcodes.I2B)
      case I2C(_)           ⇒ mv.visitInsn(Opcodes.I2C)
      case I2D(_)           ⇒ mv.visitInsn(Opcodes.I2D)
      case I2F(_)           ⇒ mv.visitInsn(Opcodes.I2F)
      case I2L(_)           ⇒ mv.visitInsn(Opcodes.I2L)
      case I2S(_)           ⇒ mv.visitInsn(Opcodes.I2S)
      case F2D(_)           ⇒ mv.visitInsn(Opcodes.F2D)
      case F2I(_)           ⇒ mv.visitInsn(Opcodes.F2I)
      case F2L(_)           ⇒ mv.visitInsn(Opcodes.F2L)
      case D2F(_)           ⇒ mv.visitInsn(Opcodes.D2F)
      case D2I(_)           ⇒ mv.visitInsn(Opcodes.D2I)
      case D2L(_)           ⇒ mv.visitInsn(Opcodes.D2L)
      case L2D(_)           ⇒ mv.visitInsn(Opcodes.L2D)
      case L2F(_)           ⇒ mv.visitInsn(Opcodes.L2F)
      case L2I(_)           ⇒ mv.visitInsn(Opcodes.L2I)
    }
  }

}
