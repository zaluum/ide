package org.zaluum.nide.compiler

import org.zaluum.nide.model.Model
import java.io.FileOutputStream
import org.zaluum.nide.model.Example
import java.io.OutputStreamWriter
import java.io.File

object TestCompiler {
  val path = new BoxClassPath(new File("./src"), currentThread.getContextClassLoader()) 
  val outDir = "classes/"
  def compile(model:Model) {
    val comp = new Compiler(model,path)
    try{
      val result = comp.compile()
      val gen  = new Generator(result,new CodeWriter(new OutputStreamWriter(System.out)))
      gen.generate()
      val bytes = ByteCodeGen.dump(result)
      val f = new File(outDir + model.className.replace(".","/") + ".class")
      f.createNewFile;
      val os = new FileOutputStream(f)
      os.write(bytes);
      os.flush
      os.close
      //new File("bin/")
    }catch {
      case e:Exception => 
        e.printStackTrace
        println (comp.reporter.errors)
    }
  }
  def main(args:Array[String]) {
    compile(Example.sumsumModel)
    compile(Example.printModel)
  }
}