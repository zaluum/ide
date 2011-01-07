package graystone.zaluum

import scala.tools.nsc.interpreter.AbstractFileClassLoader
import scala.tools.nsc.util.BatchSourceFile
import java.io.PrintWriter
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.Global
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Settings
import scala.tools.nsc.io.VirtualDirectory
import javax.script.SimpleScriptContext
object TestScript {
 private def reporter(settings: Settings) =
    new ConsoleReporter(settings, null, new java.io.PrintWriter(Console.err))

  private def classpath = System.getProperty("java.class.path")

  private def settings = {
    val s = new Settings
    s.classpath.value = classpath
    s
  }
  def main(args : Array[String]) {
    val outdir = new VirtualDirectory("outdir", None)
    val settings = new Settings 
    val out = new PrintWriter(System.err)
    settings.outputDirs setSingleOutput outdir
    object reporter extends ConsoleReporter(settings, null, out) {
    override def printMessage(msg: String) {
      out println msg
      out.flush()
    }
  }
   val compiler = new Global(settings, reporter)
   val run = new compiler.Run()
    run.compileSources(List(new BatchSourceFile("l", """class d{def p(){println("hola")}} """)))
    run
    val cl = new AbstractFileClassLoader(outdir,Thread.currentThread.getContextClassLoader)
    val cla = cl.loadClass("d")
    val a = cla.newInstance.asInstanceOf[{def p()}]
    a.p()
    
  /*val env = new Settings(println(_)) 
  env.usejavacp.value = true 
  val p = new Interpreter(env) 
  p.setContextClassLoader 
  println(p.eval[Int]("2+2"));*/
  }
}