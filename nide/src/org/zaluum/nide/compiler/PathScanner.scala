package org.zaluum.nide.compiler

import graystone.zaluum.annotations.{In,Out,Box=>BoxAnn}
import org.objectweb.asm.ClassReader
import java.net.URL
import java.io.File
import java.io.FileInputStream
import org.zaluum.nide.model._
import com.impetus.annovention._
import com.impetus.annovention.listener._
import scala.util.control.Exception._
object PathScanner {
  def main(args: Array[String]) {
    new BoxClassPath(new File("."))
  }
}
class BoxClassPath(zaluumDir: File) {
  var cache = Map[String,BoxClass]()
  
  if (!zaluumDir.isDirectory) throw new java.io.IOException()

  def find(str: String) :Option[BoxClass] =  {
    cache.get(str) orElse findZaluum(str) orElse findClass(str)
  }

  def findZaluum(str:String)  = {
    val fn= str.replace('.','/')
    val f = new File(zaluumDir.getAbsolutePath + "/" + fn + ".zaluum")
    addCache(readZaluum(f)) 
  }
  def forName(str:String) : Option[Class[_]] = {
    try { Some(Class.forName(str)) }
    catch{ case e:Exception=> e.printStackTrace; None }
  }
  def findClass(str:String) = {
    forName(str) flatMap { 
      c=>  addCache(classToBoxClass(c))  
    }
  }
  def addCache(c : Option[BoxClass]) =  {
    c foreach { bc => cache += (bc.className -> bc) }
    c
  }
  
  def classToBoxClass(cl:Class[_]) :Option[BoxClass] = {
    cl.getAnnotations().find {a => a.isInstanceOf[BoxAnn]} map { _=>
        val bc = new BoxClass(cl.getName)
        println("found class" + cl.getName)
        for (f <- cl.getFields()) {
          f.getAnnotations() foreach { 
            _ match {
              case _:In =>  bc.ports += TypedPort(f.getType.toString, true, f.getName)
              case _:Out=>  bc.ports += TypedPort(f.getType.toString, false, f.getName)
            }
          }
        }
        bc
    }
  }
  def readZaluum(f: File): Option[BoxClass] = {
    try {
      val in = new FileInputStream(f)
      try{
        val d = ProtoModel.readDefinition(in)
        println("discovered zaluum " + f.getName + " " + d)
        Some(d)
      }finally{in.close()}
    } catch {
      case e:Exception => None 
    } 
  }
}
class BoxClassPathScanner(zaluumDir: File) extends BoxClassPath(zaluumDir){
  // TODO weak hashmap on demand find

  var zaluums = Map[String, BoxClass]()
  var javaClasses = Map[String, BoxClass]()
  scanZaluums()
  scanClassPath()

  private def scanZaluums() {
    if (zaluumDir.isDirectory) {
      for (f ← zaluumDir.listFiles if f.getName.endsWith(".zaluum")) {
        addCache(readZaluum(f)) foreach { bc => zaluums += (bc.className -> bc) } 
      }
    }
  }
  private def scanClassPath() {
    val discoverer = new Discoverer() {
      def findResources() = {
        Array(new File("./bin").toURL)
      }
      def getFilter = new FilterImpl()
    }
    // discoverer.setFilter(new FilterImpl());
    // Register class annotation listener

    val inStr = classOf[graystone.zaluum.annotations.In].getName
    val outStr = classOf[graystone.zaluum.annotations.Out].getName
    discoverer.addAnnotationListener(new ClassAnnotationDiscoveryListener() {
      def discovered(clazz: String, annotation: String) {
        println("Discovered Class(" + clazz + ") " + "with Annotation(" + annotation + ")");
        val cl = new BoxClass(clazz);
        addCache(Some(cl))
        javaClasses += (clazz -> cl)
      }
      def supportedAnnotations() = Array(classOf[graystone.zaluum.annotations.Box].getName)
    });
    discoverer.addAnnotationListener(new FieldAnnotationDiscoveryListener() {
      def discovered(clazz: String, field: String, descriptor: String, annotation: String) {
        javaClasses.get(clazz) foreach { bc ⇒
          bc.ports += TypedPort(descriptor, annotation == inStr, field)
        }
        println("clazz: " + clazz + " field " + field + " descriptor " + descriptor + " annotation " + annotation)
      }
      def supportedAnnotations() = Array(inStr, outStr)
    });

    // Fire it
    discoverer.discover();
    println("end")
  }

}