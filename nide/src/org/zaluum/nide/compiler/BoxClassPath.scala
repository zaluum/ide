package org.zaluum.nide.compiler

import org.zaluum.nide.java.Widget
import com.impetus.annovention._
import com.impetus.annovention.listener._
import java.io.{ FileInputStream, File }
import java.net.URL
import org.zaluum.nide.java.{ In, Out, Box ⇒ BoxAnn, BoxImage }
import org.zaluum.nide.model._
import scala.reflect.ScalaSignature
import scala.util.control.Exception._

trait BoxClassPath {
  def find(bcr : BoxClassName) : Option[BoxClass]
  //def find(str: String): Option[BoxClass]
  def getResource(str: String): Option[URL]
}
trait ScannedBoxClassPath extends BoxClassPath {
  def boxClasses: Set[BoxClass]
  def update()
}
abstract class ChainedBoxClassPath(parent:BoxClassPath) extends BoxClassPath {
  protected def classes : Set[BoxClass]
  def find(bcn : BoxClassName) = classes.find(_.className == bcn) orElse parent.find(bcn)
  def getResource(str:String) = parent.getResource(str)
}

abstract class ChainedScannedBoxClassPath(parent:ScannedBoxClassPath) 
  extends ChainedBoxClassPath(parent) with ScannedBoxClassPath {
 def boxClasses = parent.boxClasses ++ classes 
 def update() = parent.update()
}
/**
 * ClassPath
 *
class SimpleBoxClassPath(zaluumDir: File, classLoader: ClassLoader) extends BoxClassPath {
  var cache = Map[String, BoxClass]()

  if (!zaluumDir.isDirectory) throw new java.io.IOException()
  /** 
   * Use this
   */
  def find(className: String): Option[BoxClass] = {
    cache.get(className) orElse
      loadZaluum(className) orElse
      findClass(className) orElse
      { println(className + " not found"); None }
  }
  def zaluumSourceFile(className: String): File = {
    val fn = className.replace('.', '/')
    new File(zaluumDir.getAbsolutePath + "/" + fn + ".zaluum")
  }
  def zaluumSourceURL(className: String): Option[URL] = {
    val f = zaluumSourceFile(className)
    toURL(f)
  }
  def toURL(f: File): Option[URL] = if (f.exists) Some(f.toURI.toURL) else None
  def getResource(str: String): Option[URL] = {
    val f = new File(zaluumDir.getAbsolutePath + "/" + str)
    if (f.exists) Some(f.toURI.toURL)
    else {
      Option(classLoader.getResource(str))
    }
  }
  def loadZaluum(className: String) = {
    addCache(readZaluum(zaluumSourceFile(className), className))
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
    cl.getAnnotations().view.collect { case a: BoxAnn => a }.map { bann:BoxAnn ⇒
      val imageAnn = cl.getAnnotations.view collect { case a: BoxImage ⇒ a.value } headOption
      val guiAnn = cl.getAnnotations.view collect { case a: Widget => a.value } headOption
      val guiClass = guiAnn flatMap { forName(_)}
      val scala = cl.getAnnotation(classOf[ScalaSignature]) != null
      val bc = new BoxClass(cl.getName, scala, imageAnn.getOrElse(""),guiClass)
      for (f ← cl.getDeclaredFields()) {
        f.getAnnotations() foreach {
          _ match {
            case in: In ⇒
              bc.ports += TypedPort(f.getType.toString, true, f.getName, Point(in.x, in.y))
            case out: Out ⇒
              bc.ports += TypedPort(f.getType.toString, false, f.getName, Point(out.x, out.y))
          }
        }
      }
      bc
    }.headOption;
  }
  def readZaluum(f: File, className: String): Option[BoxClass] = {
    try {
      val in = new FileInputStream(f)
      try {
        val d = ProtoModel.readDefinition(in, className)
        println("discovered zaluum " + f.getName + " " + d)
        Some(d)
      } finally { in.close() }
    } catch {
      case e: Exception ⇒ None
    }
  }
}
class SimpleScannedBoxClassPath(zaluumDir: File, cl: ClassLoader) extends SimpleBoxClassPath(zaluumDir, cl) with ScannedBoxClassPath {
  def update() {
    cache = cache.empty
    scanZaluums()
    scanClassPath()
  }
  def boxClasses = cache.values.toSet;
  private def scanZaluums() {
    if (zaluumDir.isDirectory) {
      for (f ← zaluumDir.listFiles if f.getName.endsWith(".zaluum")) {
        addCache(readZaluum(f, "nono")) // FIXME
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
        val cl = new BoxClass(clazz, false, "null", None); // FIXME
        addCache(Some(cl))
      }
      def supportedAnnotations() = Array(classOf[Box].getName)
    });
    discoverer.addAnnotationListener(new FieldAnnotationDiscoveryListener() {
      def discovered(clazz: String, field: String, descriptor: String, annotation: String) {
        cache.get(clazz) foreach { bc ⇒
          bc.ports += TypedPort(descriptor, annotation == inStr, field, Point(0, 0)) // FIXME
        }
        println("clazz: " + clazz + " field " + field + " descriptor " + descriptor + " annotation " + annotation)
      }
      def supportedAnnotations() = Array(inStr, outStr)
    });

    // Fire it
    discoverer.discover();
    println("end")
  }

}*/