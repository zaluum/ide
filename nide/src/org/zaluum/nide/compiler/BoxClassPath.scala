package org.zaluum.nide.compiler

import scala.reflect.ScalaSignature
import org.zaluum.nide.java.{ In, Out, Box ⇒ BoxAnn }
import org.objectweb.asm.ClassReader
import java.net.URL
import java.io.File
import java.io.FileInputStream
import org.zaluum.nide.model._
import com.impetus.annovention._
import com.impetus.annovention.listener._
import scala.util.control.Exception._

trait BoxClassPath {
  def find(str: String): Option[BoxClass]
  def getResource(str:String) : Option[URL]
}
trait ScannedBoxClassPath extends BoxClassPath {
  def boxClasses : Set[BoxClass]
}
/**
 * ClassPath
 */
class SimpleBoxClassPath(zaluumDir: File, classLoader: ClassLoader) extends BoxClassPath{
  var cache = Map[String, BoxClass]()

  if (!zaluumDir.isDirectory) throw new java.io.IOException()
  /** 
   * Use this
   */
  def find(str: String): Option[BoxClass] = {
    cache.get(str) orElse
      findZaluum(str) orElse
      findClass(str) orElse
      { println(str + " not found"); None }
  }
  def getResource(str:String) : Option[URL] = {
    val f = new File(zaluumDir.getAbsolutePath + "/" + str)
    if (f.exists) Some(f.toURI.toURL)
    else {
      Option(classLoader.getResource(str))
    }
  }
  def findZaluum(str: String) = {
    val fn = str.replace('.', '/')
    val f = new File(zaluumDir.getAbsolutePath + "/" + fn + ".zaluum")
    addCache(readZaluum(f))
  }

  def findClass(str: String) = {
    forName(str) flatMap { c ⇒
      addCache(classToBoxClass(c))
    }
  }
  def forName(str: String): Option[Class[_]] = {
    try { Some(classLoader.loadClass(str)) }
    catch { case e: Exception ⇒ e.printStackTrace; None }
  }
  def addCache(c: Option[BoxClass]) = {
    c foreach { bc ⇒ cache += (bc.className -> bc) }
    c
  }

  def classToBoxClass(cl: Class[_]): Option[BoxClass] = {
    cl.getAnnotations().find { a ⇒ a.isInstanceOf[BoxAnn] } map { ann ⇒
      val bann = ann.asInstanceOf[BoxAnn] 
      val scala = cl.getAnnotation(classOf[ScalaSignature])!=null
      val bc = new BoxClass(cl.getName,scala,bann.image)
      for (f ← cl.getDeclaredFields()) {
        f.getAnnotations() foreach {
          _ match {
            case in: In ⇒ 
              bc.ports += TypedPort(f.getType.toString, true, f.getName, Point(in.x,in.y))
            case out: Out ⇒
              bc.ports += TypedPort(f.getType.toString, false, f.getName, Point(out.x,out.y))
          }
        }
      }
      bc
    }
  }
  def readZaluum(f: File): Option[BoxClass] = {
    try {
      val in = new FileInputStream(f)
      try {
        val d = ProtoModel.readDefinition(in)
        println("discovered zaluum " + f.getName + " " + d)
        Some(d)
      } finally { in.close() }
    } catch {
      case e: Exception ⇒ None
    }
  }
}
class SimpleScannedBoxClassPath(zaluumDir: File, cl: ClassLoader) extends SimpleBoxClassPath(zaluumDir, cl) with ScannedBoxClassPath{

  scanZaluums()
  scanClassPath()
  
  def boxClasses = cache.values.toSet;
  private def scanZaluums() {
    if (zaluumDir.isDirectory) {
      for (f ← zaluumDir.listFiles if f.getName.endsWith(".zaluum")) {
        addCache(readZaluum(f)) 
      }
    }
  }
  private def scanClassPath() {
    val discoverer = new Discoverer() {
      def findResources() = {
        Array(new File("./bin").toURI.toURL) // FIXME use classloader to explore instead of bin 
      }
      def getFilter = new FilterImpl()
    }
    // discoverer.setFilter(new FilterImpl());
    // Register class annotation listener

    val inStr = classOf[In].getName
    val outStr = classOf[Out].getName
    discoverer.addAnnotationListener(new ClassAnnotationDiscoveryListener() {
      def discovered(clazz: String, annotation: String) {
        println("Discovered Class(" + clazz + ") " + "with Annotation(" + annotation + ")");
        val cl = new BoxClass(clazz,false,"null"); // FIXME
        addCache(Some(cl))
      }
      def supportedAnnotations() = Array(classOf[Box].getName)
    });
    discoverer.addAnnotationListener(new FieldAnnotationDiscoveryListener() {
      def discovered(clazz: String, field: String, descriptor: String, annotation: String) {
        cache.get(clazz) foreach { bc ⇒
          bc.ports += TypedPort(descriptor, annotation == inStr, field,Point(0,0)) // FIXME
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