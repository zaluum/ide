package org.zaluum.nide.compiler

import org.objectweb.asm._
import Opcodes._

object ByteCodeGen {
  def dump(bd: BoxDef): Array[Byte] = {
    val sym = bd.symbol.asInstanceOf[BoxTypeSymbol]
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
    cw.visit(V1_5, ACC_PUBLIC + ACC_SUPER, sym.tpe.name.internal, null, "java/lang/Object", null);
    //val av = cw.visitAnnotation("org/zaluum/nide/java/Box", true);
    //av.visitEnd();
    cw.visitSource(sym.source, null);
    // FIELDS 
    {
      for (b ← sym.valsInOrder) {
        val fv = cw.visitField(ACC_PUBLIC, b.name.str, b.tpe.name.descriptor, null, null)
        fv.visitEnd()
      }
      for (p ← sym.portsInOrder) {
        val fv = cw.visitField(ACC_PUBLIC, p.name.str, p.tpe.name.descriptor, null, null)
        fv.visitEnd
      }
      WidgetTemplateDump.createWidgetField(cw)
      // generate init
      val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
      mv.visitCode();
      val l0 = new Label();
      mv.visitLabel(l0);
      mv.visitVarInsn(ALOAD, 0);
      mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V");

      // INIT 
      for (b ← sym.valsInOrder) {
        mv.visitVarInsn(ALOAD, 0);
        mv.visitTypeInsn(NEW, b.tpe.name.internal);
        mv.visitInsn(DUP);
        mv.visitMethodInsn(INVOKESPECIAL, b.tpe.name.internal, "<init>", "()V");
        mv.visitFieldInsn(PUTFIELD, sym.tpe.name.internal, b.name.str, b.tpe.name.descriptor);
      }
      for (p ← sym.portsInOrder) {
        // TODO init values
      }
      // Create widget
      WidgetTemplateDump.attachInitCode(mv, sym)

      mv.visitInsn(RETURN);
      val l5 = new Label();
      mv.visitLabel(l5);
      mv.visitLocalVariable("this", sym.tpe.name.descriptor, null, l0, l5, 0);
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
      def loadBox(b: ValSymbol): Unit = {
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, sym.tpe.name.internal, b.name.str, b.tpe.name.descriptor);
      }
      def getField(className: Name, name: String, desc: String, scala: Boolean) {
        if (scala) mv.visitMethodInsn(INVOKEVIRTUAL, className.internal, name, "()" + desc);
        else mv.visitFieldInsn(GETFIELD, className.internal, name, desc);
      }
      def putField(className: Name, name: String, desc: String, scala: Boolean) {
        if (scala) mv.visitMethodInsn(INVOKEVIRTUAL, className.internal, name + "_$eq", "(" + desc + ")V");
        else mv.visitFieldInsn(PUTFIELD, className.internal, name, desc);
      }
      def putRef(pref: PortRef, get: ⇒ Unit) = {
        val portSym = pref.symbol.asInstanceOf[PortSymbol]
        pref.fromRef match {
          case bref: ValRef ⇒
            val boxSym = bref.symbol.asInstanceOf[ValSymbol]
            loadBox(boxSym)
            get
            putField(boxSym.tpe.name, portSym.name.str, portSym.tpe.name.descriptor, false) // TODO c.boxType(b.box).scala)
          case _ ⇒
            mv.visitVarInsn(ALOAD, 0)
            get
            putField(sym.tpe.name, portSym.name.str, portSym.tpe.name.descriptor, false)
        }
      }
      def getRef(pref: PortRef) = {
        val portSym = pref.symbol.asInstanceOf[PortSymbol]
        pref.fromRef match {
          case b: ValRef ⇒
            val boxSym = b.symbol.asInstanceOf[ValSymbol]
            loadBox(boxSym)
            getField(boxSym.tpe.name, portSym.name.str, portSym.tpe.name.descriptor, false) // TODO scala
          case _ ⇒
            mv.visitVarInsn(ALOAD, 0)
            getField(sym.tpe.name, portSym.name.str, portSym.tpe.name.descriptor, false)
        }
      }
      def executeConnection(con: ConnectionSymbol) {
        putRef(con.to.asInstanceOf[PortRef], getRef(con.from.asInstanceOf[PortRef]))
      }
      def connectionsFrom(f: PortRef) = {
        sym.connections filter { _.from eq f }
      }
      def connectionsFromModel(p: PortSymbol ) = {
        sym.connections filter { _.from.symbol == p }
      }
      def connectionsFromBox(b: ValSymbol) = {
        sym.connections filter { conn ⇒
          conn.from match {
            case f: PortRef ⇒ f.fromRef == b
            case _ ⇒ false
          }
        }
      }
      // propagate inputs
      for (port ← sym.portsInOrder) {
        connectionsFromModel(port) foreach { executeConnection(_) }
      }
      for (b ← sym.executionOrder) {
        // invoke box
        mv.visitVarInsn(ALOAD, 0);
        mv.visitFieldInsn(GETFIELD, sym.tpe.name.internal, b.name.str, b.tpe.name.descriptor);
        mv.visitMethodInsn(INVOKEVIRTUAL, b.tpe.name.internal, "apply", "()V");
        // propagate
        val connections = connectionsFromBox(b)
        connections foreach { executeConnection(_) }
      }
      val lend = new Label();
      mv.visitInsn(RETURN);
      mv.visitLabel(lend);
      mv.visitLocalVariable("this", sym.tpe.name.descriptor, null, l0, lend, 0);
      mv.visitMaxs(-1, -1);
      mv.visitEnd();
    }

    cw.visitEnd();
    cw.toByteArray();
  }

}
