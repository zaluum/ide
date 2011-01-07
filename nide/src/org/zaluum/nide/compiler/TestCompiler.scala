package org.zaluum.nide.compiler

import java.io.FileOutputStream
import org.zaluum.nide.model.Example
import java.io.OutputStreamWriter
import java.io.File

object TestCompiler {
  def main(args:Array[String]) {
    val model = Example.testModel
    val path = new PathScanner(new File("."))
    val comp = new Compiler(model,path)
    try{
      val result = comp.compile()
      val gen  = new Generator(result,new CodeWriter(new OutputStreamWriter(System.out)))
      gen.generate()
      val bytes = ByteCodeGen.dump(result)
      val f = new File("classes/graystone/zaluum/SumSum.class")
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
}