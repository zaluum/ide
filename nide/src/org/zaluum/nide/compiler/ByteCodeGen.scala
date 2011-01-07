package org.zaluum.nide.compiler

import org.objectweb.asm._;

import Opcodes._

object ByteCodeGen {
  def internal(classname: String) = classname.replace('.', '/')
  def classDescriptor(classname: String) = 'L' + internal(classname) + ";"
  def dump(c: Compiled) ={
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, internal(c.m.className), null, "java/lang/Object", null);

    cw.visitSource(c.source, null);
    // FIELDS 
    {
      for (b ← c.boxesInOrder) {
        val fv = cw.visitField(ACC_PUBLIC, b.name, classDescriptor(b.className), null, null);
        fv.visitEnd();
      }
      // generate init
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
      mv.visitCode();
      val l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");

      for (b ← c.boxesInOrder) {
        mv.visitVarInsn(ALOAD, 0);
        mv.visitTypeInsn(NEW, internal(b.className));
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, internal(b.className), "<init>", "()V");
        mv.visitFieldInsn(PUTFIELD, internal(c.m.className), b.name, classDescriptor(b.className));
      }
      mv.visitInsn(RETURN);
      val l5 = new Label();
      mv.visitLabel(l5);
      mv.visitLocalVariable("this", classDescriptor(c.m.className), null, l0, l5, 0);
      mv.visitMaxs(-1, -1); // autocompute
      mv.visitEnd();
    }
    // METHOD
    {
      // TODO propagate inputs 
      // run all boxes
      val mv = cw.visitMethod(ACC_PUBLIC, "apply", "()V", null, null);
      mv.visitCode();
      val l0 = new Label();
      mv.visitLabel(l0);
      for (box ← c.order) {
        // invoke box
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, internal(c.m.className), box.name, classDescriptor(box.className));
        mv.visitMethodInsn(INVOKEVIRTUAL, internal(box.className), "apply", "()V");
        // propagate
        for (conn ← c.m.connections if (conn.from.get.box == box)) {
          val from = conn.from.get
          val to = conn.to.get
          mv.visitVarInsn(ALOAD, 0);
          mv.visitFieldInsn(GETFIELD, internal(c.m.className), to.box.name, classDescriptor(to.box.className));
          mv.visitVarInsn(ALOAD, 0);
          mv.visitFieldInsn(GETFIELD, internal(c.m.className), from.box.name, classDescriptor(from.box.className));
          mv.visitFieldInsn(GETFIELD, internal(from.box.className), from.name, "D");
          mv.visitFieldInsn(PUTFIELD, internal(to.box.className), to.name, "D");
        }

        val lend = new Label();
        mv.visitInsn(RETURN);
        mv.visitLabel(lend);
        mv.visitLocalVariable("this", classDescriptor(c.m.className), null, l0, lend, 0);
        mv.visitMaxs(-1, -1);
        mv.visitEnd();
      }
    }
    cw.visitEnd();
    cw.toByteArray();
  }
}
