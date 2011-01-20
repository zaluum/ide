package org.zaluum.nide.compiler
import java.io.PrintWriter
import org.zaluum.nide.model.BoxPortRef

class Generator(c: Compiled, out: CodeWriter) {
  def generate() {
    out.println("package pkg;")
    out.println("public class " + c.m.className + "{")
    out.indent()
    generateFields();
    generateAct()
    out.undent()
    out.println("}")
    out.flush()
  }
  def generateFields() {
    for (b ← c.boxesInOrder) {
      out.println("public " + b.className + " " + b.name + " = new " + b.className + "();")
    }
  }
  def generateAct() {
    out.println("public void act() {");
    out.indent()
    // TODO propagate inputs 
    // run all boxes
    for (box ← c.order) {
      out.println(box.name + ".act();")
      for (conn ← c.m.connections) {
        (conn.from, conn.to) match {
          case (Some(from: BoxPortRef), Some(to: BoxPortRef)) if (from.box == box) ⇒
            out.println(to.box.name + "." + to.name + "=" + from.box.name + "." + from.name + ";")
          case _ ⇒
        }
      }
    }

    out.undent()
    out.println("}");
  }
}