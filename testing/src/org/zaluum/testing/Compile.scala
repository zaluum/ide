package org.zaluum.testing
import org.eclipse.jdt.core.compiler.batch.BatchCompiler
import java.io.PrintWriter
import java.io.File
import java.io.FileFilter

object Compile {
  val rt = "/home/frede/devel/jdk1.7.0/jre/lib/rt.jar"
  val embedded = new File("/home/frede/devel/zaluum/embedded-lib/lib/")
  val libs = if (embedded.exists && embedded.isDirectory) {
    embedded.listFiles() filter { _.getName.endsWith(".jar")} map {_.getAbsolutePath} toList 
  }else List()
  def runTest(path:String) {
    val src = path + "/src"
    val cp = (src :: rt :: libs)
    val target = path + "/target"
    val options = "-d " + target +
        " -cp " + cp.mkString(":") + " test1/src/Test1.java test1/src/Test2.zaluum";
    println(options)
    BatchCompiler.compile(options, new PrintWriter(System.out), new PrintWriter(System.err), null);    
  }
  def main(args:Array[String]){
    runTest("test1")
  }
}