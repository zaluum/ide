package org.zaluum.testing
import org.eclipse.jdt.core.compiler.batch.BatchCompiler
import java.io.PrintWriter
import java.io.File
import java.io.FileFilter

object Compile {
  def main(args:Array[String]){
    val rt = "/home/frede/devel/jdk1.7.0/jre/lib/rt.jar"
    val embedded = new File("/home/frede/devel/zaluum/embedded-lib/lib/")
    val libs = if (embedded.exists && embedded.isDirectory) {
      embedded.listFiles() filter { _.getName.endsWith(".jar")} mkString (":")
    }else ""
    BatchCompiler.compile("-verbose -d testout -cp test1:" + libs + ":" + rt + " test1/Test1.java test1/Test2.zaluum", new PrintWriter(System.out), new PrintWriter(System.err), null);
  }
}