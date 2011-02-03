package org.zaluum.nide.newcompiler

import org.objectweb.asm._

import Opcodes._
import ByteCodeGen._
object WidgetTemplateDump {

  def createWidgetField(cw: ClassWriter) {
    val fv = cw.visitField(ACC_PUBLIC, "widget", "Ljavax/swing/JComponent;", null, null);
    fv.visitEnd();
  }
  def attachInitCode(mv: MethodVisitor, sym: BoxTypeSymbol ) {
    mv.visitVarInsn(ALOAD, 0);
    mv.visitTypeInsn(NEW, "javax/swing/JPanel");
    mv.visitInsn(DUP);
    mv.visitInsn(ACONST_NULL);
    mv.visitMethodInsn(INVOKESPECIAL, "javax/swing/JPanel", "<init>", "(Ljava/awt/LayoutManager;)V");
    mv.visitFieldInsn(PUTFIELD, sym.tpe.name.internal, "widget", "Ljavax/swing/JComponent;");
    mv.visitVarInsn(ALOAD, 0);
    mv.visitFieldInsn(GETFIELD, sym.tpe.name.internal, "widget", "Ljavax/swing/JComponent;");
    // TODO mv.visitIntInsn(SIPUSH, c.bcd.guiSize.w); // guisize
    // TODO mv.visitIntInsn(SIPUSH, c.bcd.guiSize.h);
    mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "setSize", "(II)V");
    for (box ← sym.valsInOrder; val typ = box.tpe.asInstanceOf[BoxTypeSymbol]; if (false)){ // TODO typ.visual)) {
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, sym.tpe.name.internal, box.name.str , box.tpe.name.descriptor);
      mv.visitFieldInsn(GETFIELD, box.tpe.name.internal, "widget", "Ljavax/swing/JComponent;");
      /*TODO mv.visitIntInsn(SIPUSH, box.guiPos.map(_.pos.x).getOrElse(0)) // TODO hardcoded
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.pos.y).getOrElse(0))
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.size.w).getOrElse(50))
      mv.visitIntInsn(SIPUSH, box.guiPos.map(_.size.h).getOrElse(50))*/
      mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "setBounds", "(IIII)V");
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, sym.tpe.name.internal, "widget", "Ljavax/swing/JComponent;");
      mv.visitVarInsn(ALOAD, 0);
      mv.visitFieldInsn(GETFIELD, sym.tpe.name.internal, box.name.str, box.tpe.name.descriptor);
      mv.visitFieldInsn(GETFIELD, box.tpe.name.internal, "widget", "Ljavax/swing/JComponent;");
      mv.visitMethodInsn(INVOKEVIRTUAL, "javax/swing/JComponent", "add", "(Ljava/awt/Component;)Ljava/awt/Component;");
      mv.visitInsn(POP);
    }
  }
}
