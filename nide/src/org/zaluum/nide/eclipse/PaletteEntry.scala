package org.zaluum.nide.eclipse
import org.zaluum.nide.compiler.Name
import scala.collection.mutable.Buffer
import java.io.InputStream
import java.io.FileInputStream
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.core.runtime.IProgressMonitor
import org.zaluum.annotation.Box
import org.zaluum.nide.compiler.Expressions
import org.eclipse.swt.dnd.ByteArrayTransfer
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.TransferData
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream
import java.io.ObjectInputStream
import java.io.ByteArrayInputStream
import org.eclipse.jdt.core.Flags
import org.zaluum.annotation.Apply
import java.util.ArrayList
import java.net.URL

case class PaletteEntry(
    className: Name,
    selector: Option[String],
    parameterTypes: List[String],
    static: Boolean,
    imagePath: Option[String],
    name: Option[String] = None,
    template: Boolean = false) {
  def split = className.str.splitAt(className.str.lastIndexOf("."))
  def pkgName = split._1
  def simpleName = split._2.drop(1)
}
object PaletteTransfer extends ByteArrayTransfer {
  val typeName = "paletteTransfer:" + System.currentTimeMillis() + ":" + PaletteTransfer.hashCode
  val typeID = Transfer.registerType(typeName);
  override protected def getTypeIds = Array(typeID)
  override protected def getTypeNames = Array(typeName)

  override protected def javaToNative(data: Object, transferData: TransferData) =
    data match {
      case p: PaletteEntry ⇒
        val b = new ByteArrayOutputStream()
        val o = new ObjectOutputStream(b)
        o.writeObject(p)
        super.javaToNative(b.toByteArray, transferData)
    }

  override protected def nativeToJava(transferData: TransferData) = {
    val bytes = super.nativeToJava(transferData).asInstanceOf[Array[Byte]]
    try {
      new ObjectInputStream(new ByteArrayInputStream(bytes)).readObject()
    } catch { case e: Exception ⇒ e.printStackTrace; null }
  }
}
class Palette(project: IJavaProject, monitor: IProgressMonitor) {
  import org.zaluum.nide.utils.JDTUtils._
  def packages: Array[String] = map.keys.toArray
  def packageChildren(pkg: String): Array[PaletteEntry] = map.get(pkg) match {
    case Some(l) ⇒ l.toArray
    case None    ⇒ Array()
  }
  def add(e: PaletteEntry) = {
    val pkg = e.pkgName
    map.get(pkg) match {
      case Some(l) ⇒ map += (pkg -> (e :: l))
      case None    ⇒ map += (pkg -> List(e))
    }
  }
  var map: Map[String, List[PaletteEntry]] = Map();
  //init
  {
    val urls = ProjectClassLoader.getClasspathUrls(project)
    for (url ← urls) {
      println("processing url" + url)
      try {
        val url2 = new URL(url.toExternalForm + "META-INF/palette.xml")
        val stream = url2.openStream()
        println(url2)
        try
          Palette.load(stream) foreach add
        finally
          stream.close
      } catch { case e: Exception ⇒ }
    }
    val list =
      search(
        patternAnnotation(classOf[Box].getName),
        projectScope(project),
        monitor)
        .flatMap(Palette.load(_)).foreach { add }
  }
}

object Palette {
  def load(t: IType): Array[PaletteEntry] = {
    val classname = Name(t.getFullyQualifiedName)
    val methodEntries = for (m ← t.getMethods(); if (m.getAnnotations.exists(_.getElementName() == classOf[Apply].getName))) yield {
      PaletteEntry(
        classname,
        Some(m.getElementName),
        m.getParameterTypes.toList,
        Flags.isStatic(m.getFlags),
        None,
        None,
        Expressions.isTemplateExpression(classname))
    }
    if (methodEntries.isEmpty)
      Array(PaletteEntry(
        classname,
        None,
        List(),
        false,
        None,
        None,
        Expressions.isTemplateExpression(classname)))
    else methodEntries

  }
  def load(file: InputStream): List[PaletteEntry] = {
    try {
      val node = xml.XML.load(file)
      var entries = Buffer[PaletteEntry]()
      for (entry ← node.child) {
        entry match {
          case <entry/> ⇒ processEntry(entry).foreach { entries += _ }
          case _        ⇒
        }
      }
      entries.toList
    } catch {
      case e: Exception ⇒
        println("Error parsing palette file " + file + e)
        List()
    }
  }
  def textOption(s: String) = s match {
    case null ⇒ None
    case ""   ⇒ None
    case o    ⇒ Some(o)
  }
  def processEntry(entry: xml.Node): Option[PaletteEntry] = {
    val className = textOption((entry \ "@class").text).map(Name(_))
    if (className.isEmpty) None
    else {
      val selector = textOption((entry \ "@selector").text.trim)
      val parameters = (entry \ "@parameters").text.split(",").map { _.trim }.filterNot(_ == "").toList
      val name = textOption((entry \ "@name").text)
      val static = (entry \ "@name").text.trim.toLowerCase() == "true"
      val imagePath = textOption((entry \ "@image").text)
      val template = (entry \ "@template").text.trim.toLowerCase() == "true"
      Some(PaletteEntry(className.get, selector, parameters, static, imagePath, name, template))
    }
  }
  /*  def main(args: Array[String]) {
    println("hola")
    println(load(new FileInputStream("testPalette.xml")))
  }
  val example =
    <palette>
      <entry class="java.lang.Math" selector="abs" static="true" parameters="int"/>
      <entry class="java.lang.Math2" selector="abs2"/>
    </palette>*/
}