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
    mv.visitVarInsn(ALOAD, 0);
    mv.visitTypeInsn(NEW, "javax/swing/JPanel");
    mv.visitInsn(DUP);
    mv.visitInsn(ACONST_NULL);
    mv.visitMethodInsn(INVOKESPECIAL, "javax/swing/JPanel", "<init>", "(Ljava/awt/LayoutManager;)V");
    mv.visitFieldInsn(PUTFIELD, internal(c.bcd.className ), "widget", "Ljavax/swing/JComponent;");
    mv.visitVarInsn(ALOAD, 0);
    mv.visitFieldInsn(GETFIELD, internal(c.bcd.className ), "widget", "Ljavax/swing/JComponent;");
    mv.visitIntInsn(SIPUSH, c.bcd.guiSize.w);
    mv.visitIntInsn(SIPUSH, c.bcd.guiSize.h);
    mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "setSize", "(II)V");
    for (box ← c.boxesInOrder; val typ = c.boxType(box); if (typ.visual)) {
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, internal(c.bcd.className ), box.name , classDescriptor(className(box)));
      mv.visitFieldInsn(GETFIELD, internal(className(box)), "widget", "Ljavax/swing/JComponent;");
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.pos.x).getOrElse(0)) // TODO hardcoded
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.pos.y).getOrElse(0))
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.size.w).getOrElse(50))
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.size.h).getOrElse(50))
      mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "setBounds", "(IIII)V");
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, internal(c.bcd.className ), "widget", "Ljavax/swing/JComponent;");
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, internal(c.bcd.className ), box.name, classDescriptor(className(box)));
      mv.visitFieldInsn(GETFIELD, internal(className(box)), "widget", "Ljavax/swing/JComponent;");
      mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "add", "(Ljava/awt/Component;)Ljava/awt/Component;");
      mv.visitInsn(POP);
    }
  }
}
