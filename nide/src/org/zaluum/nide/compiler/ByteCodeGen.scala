package org.zaluum.nide.compiler

import org.objectweb.asm._
import Opcodes._
object ByteCodeGen {
  def descriptor(n: Name) = n match {
    case Name("double") ⇒ "D"
    case Name("boolean") => "Z"
    case null ⇒ "null"
    case _ ⇒ "L" + n.internal + ";"
  }
  def dump(bc: BoxClass): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, bc.name.internal, null, bc.superName.internal, null);
    var mv: MethodVisitor = null
    val thisDescriptor = descriptor(bc.name)
    def emitMethod(name: String, signature: String, tree: Tree, constructor: Boolean) {
      mv = cw.visitMethod(ACC_PUBLIC, name, signature, null, null);
      mv.visitCode();
      if (constructor) {
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, bc.superName.internal, "<init>", "()V");
      }
      val l0 = new Label();
      mv.visitLabel(l0);
      tree.children foreach { emit(_) }
      val lend = new Label();
      mv.visitInsn(RETURN);
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", thisDescriptor, null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    def emit(tree: Tree): Unit = {
      tree match {
        case EmptyTree ⇒
        case FieldDef(name, tpe) ⇒
          cw.visitField(ACC_PUBLIC, name.str, descriptor(tpe), null, null).visitEnd()
        case ConstructorMethod(c) ⇒
          emitMethod("<init>", "()V", tree, true)
        case Method(name, signature, stats) ⇒
          emitMethod(name.str, signature, tree, false)
        case New(typeName, param, signature) ⇒
          mv.visitTypeInsn(NEW, typeName.internal);
          mv.visitInsn(DUP);
          emit(param)
          mv.visitMethodInsn(INVOKESPECIAL, typeName.internal, "<init>", signature);
        case NullConst ⇒
          mv.visitInsn(ACONST_NULL)
        case Assign(lhs, rhs) ⇒
          lhs match {
            case Select(a, FieldRef(id, typeName, fromClass)) ⇒
              emit(a)
              emit(rhs)
              mv.visitFieldInsn(PUTFIELD, fromClass.internal, id.str, descriptor(typeName))
          }
        case Select(a, b) ⇒
          emit(a)
          emit(b)
        case FieldRef(id, typeName, fromClass) ⇒
          mv.visitFieldInsn(GETFIELD, fromClass.internal, id.str, descriptor(typeName))
        case This ⇒
          mv.visitVarInsn(ALOAD, 0);
        case Invoke(obj, meth, param, fromClass, descriptor) ⇒
          emit(obj)
          param foreach { emit(_) }
          mv.visitMethodInsn(INVOKEVIRTUAL, fromClass.internal, meth, descriptor)
        case Pop ⇒
          mv.visitInsn(POP)
        case Const(i) ⇒
          mv.visitIntInsn(SIPUSH, i)
        case Return(t) =>
          emit(t)
          mv.visitInsn(IRETURN)
        case True => 
          mv.visitInsn(ICONST_1)
      }
    }
    bc.children foreach { emit(_) }
    cw.visitEnd()
    cw.toByteArray
  }

}
