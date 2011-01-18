package org.zaluum.nide.compiler

import org.zaluum.nide.model.ProtoModel
import org.zaluum.nide.model.Model
import java.io.FileOutputStream
import org.zaluum.nide.model.Example
import java.io.OutputStreamWriter
import java.io.File

object TestCompiler {
  val path = new SimpleBoxClassPath(new File("./src"), currentThread.getContextClassLoader()) 
  val outDir = "classes/"
  def compile(str:String) {
    path.getResource(str) match {
      case Some(url) =>
        val i = url.openStream
        val model = ProtoModel.read(i)
        i.close
        compile(model)
      case None => println("File " + str + " not found")
    }
  }
  def compile(model:Model) {
    val comp = new Compiler(model,path)
    try{
      val result = comp.compile()
      //val gen  = new Generator(result,new CodeWriter(new OutputStreamWriter(System.out)))
      //gen.generate()
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
        println (comp.reporter.errors)
        throw e
    }
  }
  def main(args:Array[String]) {
    compile("graystone/zaluum/SumSum.zaluum")
    compile(Example.printModel)
    compile(Example.encapsulatePrint)
  }
}