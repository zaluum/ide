package org.zaluum.nide.compiler
import org.objectweb.asm._
import Opcodes._
import org.zaluum.nide.model.{ Box, PortRef, ModelPortRef, BoxPortRef, Connection }

object ByteCodeGen {
  private def internal(classname: String) = classname.replace('.', '/')
  private def classDescriptor(classname: String) = 'L' + internal(classname) + ";"
  def dump(c: Compiled): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);

    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, internal(c.m.className), null, "java/lang/Object", null);
    val av = cw.visitAnnotation("org.zaluum.nide.java.Box", true);
    av.visitEnd();
    cw.visitSource(c.source, null);
    // FIELDS 
    {
      for (b ← c.boxesInOrder) {
        val fv = cw.visitField(ACC_PUBLIC, b.name, classDescriptor(b.className), null, null)
        fv.visitEnd()
      }
      for (p ← c.portDeclInOrder) {
        val fv = cw.visitField(ACC_PUBLIC, p.name, p.descriptor , null, null)
        fv.visitEnd
      }
      // generate init
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
      mv.visitCode();
      val l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");

      // INIT 
      for (b ← c.boxesInOrder) {
        mv.visitVarInsn(ALOAD, 0);
        mv.visitTypeInsn(NEW, internal(b.className));
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, internal(b.className), "<init>", "()V");
        mv.visitFieldInsn(PUTFIELD, internal(c.m.className), b.name, classDescriptor(b.className));
      }
      for (p ← c.portDeclInOrder) {
        // TODO init values
      }
      mv.visitInsn(RETURN);
      val l5 = new Label();
      mv.visitLabel(l5);
      mv.visitLocalVariable("this", classDescriptor(c.m.className), null, l0, l5, 0);
      mv.visitMaxs(-1, -1); // autocompute
      mv.visitEnd();
    }
    // METHOD APPLY
    {
      // run all boxes
      val mv = cw.visitMethod(ACC_PUBLIC, "apply", "()V", null, null);
      mv.visitCode();
      val l0 = new Label();
      mv.visitLabel(l0);

      // Utility methods
      def loadBox(b: Box): Unit = {
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, internal(c.m.className), b.name, classDescriptor(b.className));
      }
      def getField(className: String, name: String, desc:String, scala: Boolean) {
        if (scala) mv.visitMethodInsn(INVOKEVIRTUAL, internal(className), name, "()"+desc);
        else mv.visitFieldInsn(GETFIELD, internal(className), name, desc);
      }
      def putField(className: String, name: String, desc: String, scala: Boolean) {
        if (scala) mv.visitMethodInsn(INVOKEVIRTUAL, internal(className), name + "_$eq", "("+desc+")V");
        else mv.visitFieldInsn(PUTFIELD, internal(className), name, desc);
      }
      def putRef(p: PortRef, get: ⇒ Unit) = p match {
        case b: BoxPortRef ⇒
          loadBox(b.box)
          get
          
          putField(b.box.className, b.name, c.portType(b).descriptor,c.boxType(b.box).scala)
        case m: ModelPortRef ⇒
          mv.visitVarInsn(ALOAD, 0)
          get
          putField(c.m.className, m.name,c.portType(m).descriptor, false)
      }
      def getRef(p: PortRef) = p match {
        case b: BoxPortRef ⇒
          loadBox(b.box)
          getField(b.box.className, b.name, c.portType(b).descriptor,c.boxType(b.box).scala)
        case m: ModelPortRef ⇒
          mv.visitVarInsn(ALOAD, 0)
          getField(c.m.className, m.name, c.portType(m).descriptor, false)
      }
      def executeConnection(c: Connection) {
        (c.from, c.to) match {
          case (Some(f), Some(t)) ⇒ putRef(t, getRef(f))
          case _ ⇒
        }
      }
      def connectionsFrom(f: PortRef) = {
        c.m.connections filter { _.from == Some(f) }
      }
      def connectionsFromBox(b: Box) = {
        c.m.connections filter { conn ⇒
          (conn.from, conn.to) match {
            case (Some(f: BoxPortRef), Some(t)) ⇒ f.box == b
            case _ ⇒ false
          }
        }
      }
      // propagate inputs
      for (portDecl ← c.portDeclInOrder) {
        val ref = ModelPortRef(portDecl.name)
        if (c.portType(ref).in) connectionsFrom(ref) foreach { executeConnection(_) }
      }
      for (box ← c.order) {
        // invoke box
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, internal(c.m.className), box.name, classDescriptor(box.className));
        mv.visitMethodInsn(INVOKEVIRTUAL, internal(box.className), "apply", "()V");
        // propagate
        val connections = connectionsFromBox(box)
        connections foreach { executeConnection(_) }
      }
      val lend = new Label();
      mv.visitInsn(RETURN);
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", classDescriptor(c.m.className), null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }
    cw.visitEnd();
    cw.toByteArray();
  }

}
