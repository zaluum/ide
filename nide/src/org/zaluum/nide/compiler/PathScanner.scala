package org.zaluum.nide.compiler

import org.objectweb.asm.ClassReader
import java.net.URL
import java.io.File
import java.io.FileInputStream
import org.zaluum.nide.model._
import com.impetus.annovention._
import com.impetus.annovention.listener._


object PathScanner {
  def main(args: Array[String]) {
    new PathScanner(new File("."))
  }
}

class PathScanner(zaluumDir:File) {
  // TODO weak hashmap on demand find
 
  var zaluumClasses = Map[String, BoxClass]()
  var javaClasses = Map[String, BoxClass]()
  scanZaluums()
  scanClassPath()
  
  private def readZaluum(f: File): BoxClass = {
    val in = new FileInputStream(f)
    try {
      val d = ProtoModel.readDefinition(in)
      println ("discovered zaluum " + f.getName + " " +d)
      d
    } finally { in.close() }
  }
  private def parseZaluum(f: File) {
    val boxClass = readZaluum(f)
    zaluumClasses += (boxClass.className -> boxClass)
  }
  private def scanZaluums() {
    if (zaluumDir.isDirectory) {
      for (f ← zaluumDir.listFiles if f.getName.endsWith(".zaluum")) {
        parseZaluum(f)
      }
    }
  }
  private def scanClassPath() {
    val discoverer = new Discoverer(){
      def findResources() = {
        Array(new File("./bin").toURL)
      }
      def getFilter =  new FilterImpl()
    }
   // discoverer.setFilter(new FilterImpl());
    // Register class annotation listener
    
    val inStr = classOf[graystone.zaluum.annotations.In].getName
    val outStr = classOf[graystone.zaluum.annotations.Out].getName
    discoverer.addAnnotationListener(new ClassAnnotationDiscoveryListener() {
      def discovered(clazz: String, annotation: String) {
        println("Discovered Class(" + clazz + ") " + "with Annotation(" + annotation + ")");
        javaClasses += (clazz -> new BoxClass(clazz))
      }
      def supportedAnnotations() = Array(classOf[graystone.zaluum.annotations.Box].getName)
    });
    discoverer.addAnnotationListener(new FieldAnnotationDiscoveryListener() {
      def discovered(clazz: String, field: String, descriptor: String, annotation: String) {
        javaClasses.get(clazz) foreach { bc =>
          bc.ports += TypedPort(descriptor,annotation==inStr,field)
        }
        println("clazz: " + clazz + " field " + field + " descriptor " + descriptor + " annotation " + annotation)
      }
      def supportedAnnotations() =  Array(inStr, outStr)
    });

    // Fire it
    discoverer.discover();
    println("end")
  }

}