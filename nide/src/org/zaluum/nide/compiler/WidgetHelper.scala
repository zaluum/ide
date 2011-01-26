package org.zaluum.nide.compiler

import org.objectweb.asm._

import Opcodes._
import ByteCodeGen._
object WidgetTemplateDump {

  def createWidgetField(cw: ClassWriter) {
    val fv = cw.visitField(ACC_PUBLIC, "widget", "Ljavax/swing/JComponent;", null, null);
    fv.visitEnd();
  }
  def attachInitCode(mv: MethodVisitor, c: Compiled) {
    val className = c.m .className 
    mv.visitVarInsn(ALOAD, 0);
    mv.visitTypeInsn(NEW, "javax/swing/JPanel");
    mv.visitInsn(DUP);
    mv.visitInsn(ACONST_NULL);
    mv.visitMethodInsn(INVOKESPECIAL, "javax/swing/JPanel", "<init>", "(Ljava/awt/LayoutManager;)V");
    mv.visitFieldInsn(PUTFIELD, internal(className), "widget", "Ljavax/swing/JComponent;");
    mv.visitVarInsn(ALOAD, 0);
    mv.visitFieldInsn(GETFIELD, internal(className), "widget", "Ljavax/swing/JComponent;");
    mv.visitIntInsn(SIPUSH, c.m.guiSize.w);
    mv.visitIntInsn(SIPUSH, c.m.guiSize.h);
    mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "setSize", "(II)V");
    for (box ← c.boxesInOrder; val typ = c.boxType(box); if (typ.visual)) {
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, internal(className), box.name , classDescriptor(box.className));
      mv.visitFieldInsn(GETFIELD, internal(box.className), "widget", "Ljavax/swing/JComponent;");
      mv.visitIntInsn(SIPUSH, box.guiPos.get.pos.x);
      mv.visitIntInsn(SIPUSH, box.guiPos.get.pos.y);
      mv.visitIntInsn(SIPUSH, box.guiPos.get.size.w);
      mv.visitIntInsn(SIPUSH, box.guiPos.get.size.h);
      mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "setBounds", "(IIII)V");
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, internal(className), "widget", "Ljavax/swing/JComponent;");
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, internal(className), box.name, classDescriptor(box.className));
      mv.visitFieldInsn(GETFIELD, internal(box.className), "widget", "Ljavax/swing/JComponent;");
      mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "add", "(Ljava/awt/Component;)Ljava/awt/Component;");
      mv.visitInsn(POP);
    }
  }
}
