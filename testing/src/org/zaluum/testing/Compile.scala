package org.zaluum.testing
import org.eclipse.jdt.core.compiler.batch.BatchCompiler
import java.io.PrintWriter
import java.io.File
import java.io.FileFilter
import java.io.StringWriter
import java.net.URLClassLoader
import scala.io.Source

object Compile {
  val rt = "/home/frede/devel/jdk1.7.0/jre/lib/rt.jar" // TODO remove absolute path
  val embedded = new File("/home/frede/devel/zaluum/embedded-lib/lib/")
  val libs = if (embedded.exists && embedded.isDirectory) {
    embedded.listFiles() filter { _.getName.endsWith(".jar") } map { _.getAbsolutePath } toList
  } else List()

  def runTest(path: File): Boolean = {
    def msg(msg: String) = {
      println(path + " " + msg)
    }
    def error(msg: String) = {
      println(">>>>>>>>>")
      println(path + " " + msg)
      println("<<<<<<<<<")
    }
    val src = path
    val cp = (src.getPath :: rt :: libs)
    val target = new File(path, "target")
    val filesToCompile = javasOrZaluums(path) map { f ⇒ f.getPath } mkString (" ")
    val compilationCheck = new File(path, "compilation.check");
    val options = "-source 1.5 -target 1.5 -d " + target.getPath +
      " -cp " + cp.mkString(":") + " " + filesToCompile;
    if (target.exists) target.delete
    target.mkdir
    //msg(" compiling with: \n\t" + options)
    val out = new StringWriter()
    val err = new StringWriter()
    val okComp = BatchCompiler.compile(options, new PrintWriter(System.out), new PrintWriter(err, true), null);
    out.flush
    err.flush
    val errStr = err.toString
    if (!okComp) {
      val f = new File(target, "compilation.check")
      f.createNewFile;
      val w = new PrintWriter(f, "utf-8")
      w.print(errStr)
      w.flush
      w.close
    }
    val compilationOk = if (compilationCheck.exists && okComp) {
      error("failed: Compilation ok but compilation.check present");
      false
    } else if (!compilationCheck.exists && !okComp) {
      error("failed: Compilation failed but no compilation.check is present")
      false
    } else if (compilationCheck.exists && !okComp) {
      val fromFile = Source.fromFile(compilationCheck, "utf-8").mkString

      if (fromFile != errStr) {
        error("failed: Compilation result is not equal to compilation.check")
        false
      } else {
        true
      }
    } else {
      true
    }
    // run test
    if (compilationOk) {
      val classToRun = {
        val f = new File(path, "run")
        if (f.exists) {
          val lines = Source.fromFile(f, "utf-8").getLines
          if (lines.hasNext) Some(lines.next().trim)
          else None
        } else None
      }
      classToRun match {
        case Some(c) ⇒
          try {
            val loader = new URLClassLoader(Array(target.toURI.toURL), Thread.currentThread.getContextClassLoader)
            val cl: Class[_] = loader.loadClass(c)
            val m = cl.getMethod("test")
            //msg("running ")
            m.invoke(null, Array(): _*)
            msg("ok")
            true
          } catch {
            case e ⇒
              error(" execution failed: " + e.toString)
              e.printStackTrace
              false
          }
        case None ⇒
          msg("ok (no runtime test)")
          true
      }
    } else false
  }

  def dirs(f: File): List[File] = {
    for (d ← f.listFiles.toList; if d.isDirectory) yield d
  }
  def javasOrZaluums(d: File): List[File] = {
    if (d.isDirectory) {
      d.listFiles.toList flatMap { f ⇒
        val n = f.getName;
        if (f.isFile && (n.endsWith(".java") || n.endsWith(".zaluum"))) List(f)
        else if (f.isDirectory) javasOrZaluums(f)
        else List()
      }
    } else List()
  }
  def main(args: Array[String]) {
    var run = 0;
    var ok = 0;
    for (d ← dirs(new File("."))) {
      if (runTest(d)) ok += 1
      run += 1
    }
    println("RUN: " + run + " OK: " + ok + " FAIL: " + (run - ok))

  }
}