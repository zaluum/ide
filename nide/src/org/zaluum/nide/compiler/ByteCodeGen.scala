package org.zaluum.nide.compiler

import org.objectweb.asm._
import Opcodes._
object ByteCodeGen {
  def simpleMethod(cw: ClassWriter, name: String, signature: String, thisDescriptor: String)(body: (MethodVisitor ⇒ Unit)) {
    val mv = cw.visitMethod(ACC_PUBLIC, name, signature, null, null);
    mv.visitCode();
    val l0 = new Label();
    mv.visitLabel(l0);
    body(mv)
    val lend = new Label();
    mv.visitInsn(RETURN);
    mv.visitLabel(lend);
    mv.visitLocalVariable("this", thisDescriptor, null, l0, lend, 0);
    mv.visitMaxs(-1, -1);
    mv.visitEnd();
  }
  def descriptor(n:Name) = n match {
    case Name("double") => "D"
    case null => "null"
    case _ => "L" + n.internal + ";"
  }
  def dump(bc: BoxClass): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, bc.name.internal, null, "java/lang/Object", null);
    //cw.visitSource(sym.source, null);
    // fields
    def emitField(t:Tree) = t match {
      case FieldDef(name, tpe) ⇒
        val fv = cw.visitField(ACC_PUBLIC, name.str, descriptor(tpe), null, null)
        fv.visitEnd()
    }
    bc.fields foreach { emitField(_) }
    // constructor 
    // TODO b.cons.portInit
    simpleMethod(cw, "<init>", "()V", descriptor(bc.name)) { mv ⇒
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");
      bc.cons.boxCreation foreach { b ⇒
        mv.visitVarInsn(ALOAD, 0);
        mv.visitTypeInsn(NEW, b.typeName.internal);
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, b.typeName.internal, "<init>", "()V");
        mv.visitFieldInsn(PUTFIELD, bc.name.internal, b.name.str, descriptor(b.typeName));
      }
    }
    // APPLY
    simpleMethod(cw, "apply", "()V", descriptor(bc.name)) { mv ⇒
      def loadField(f: FieldRef) = {
        //if (scala) mv.visitMethodInsn(INVOKEVIRTUAL, className.internal, name, "()" + desc);
        mv.visitFieldInsn(GETFIELD, f.fromClass.internal, f.id.str, descriptor(f.typeName));
      }
      def putField(f: FieldRef) = {
        // if (scala) mv.visitMethodInsn(INVOKEVIRTUAL, className.internal, name + "_$eq", "(" + desc + ")V");
        mv.visitFieldInsn(PUTFIELD, f.fromClass.internal, f.id.str, descriptor(f.typeName));
      }
      def loadThis = mv.visitVarInsn(ALOAD, 0)
      def get(t: Select) = t match {
        case Select(This, f: FieldRef) ⇒ loadThis; loadField(f)
        case Select(f1: FieldRef, f2: FieldRef) ⇒ loadThis; loadField(f1); loadField(f2)
      }
      def assign(to: Select, from: Select) {
        to match {
          case Select(This, _) ⇒ loadThis
          case Select(f: FieldRef, _) ⇒ loadThis; loadField(f)
        }
        get(from)
        to match {
          case Select(_, f: FieldRef) ⇒ putField(f)
        }
      }
      def emitStat(t: Tree) = t match {
        case RunBox(id, tpe) ⇒
          loadThis
          mv.visitFieldInsn(GETFIELD, bc.name.internal, id.str, descriptor(tpe));
          mv.visitMethodInsn(INVOKEVIRTUAL, tpe.internal, "apply", "()V");
        case Propagate(a:Select, b:Select) ⇒
          assign(b, a)
      }
      bc.apply.stats foreach { emitStat(_) }
    }
    cw.visitEnd()
    cw.toByteArray
  }
  

}
