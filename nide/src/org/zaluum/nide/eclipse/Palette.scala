package org.zaluum.nide.eclipse
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.lang.Object
import java.lang.System

import scala.Array.canBuildFrom
import scala.collection.mutable.Buffer

import org.eclipse.core.resources.IFolder
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.Flags
import org.eclipse.jdt.core.IJarEntryResource
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IType
import org.eclipse.swt.dnd.ByteArrayTransfer
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.TransferData
import org.zaluum.annotation.Apply
import org.zaluum.annotation.Box
import org.zaluum.nide.compiler.Expressions
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.PortDir
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.utils.JDTUtils.patternAnnotation
import org.zaluum.nide.utils.JDTUtils.projectScope
import org.zaluum.nide.utils.JDTUtils.search

class Palette(project: IJavaProject) {
  import org.zaluum.nide.utils.JDTUtils._
  def packages: Array[String] = map.keys.toArray
  def packageChildren(pkg: String): Array[PaletteEntry] = map.get(pkg) match {
    case Some(l) ⇒ l.toArray
    case None    ⇒ Array()
  }

  var map: Map[String, List[PaletteEntry]] = Map();
  //init
  def reload(monitor: IProgressMonitor) {
    map = Map()
      def add(e: PaletteEntry) = {
        val pkg = e.pkgName
        map.get(pkg) match {
          case Some(l) ⇒ map += (pkg -> (e :: l))
          case None    ⇒ map += (pkg -> List(e))
        }
      }
    for (
      r ← project.getAllPackageFragmentRoots;
      res ← r.getNonJavaResources
    ) {
      res match {
        case r: IJarEntryResource if r.getName.endsWith("META-INF") ⇒
          r.getChildren().find(_.getName() == "palette.xml").foreach { f ⇒
            val i = f.getContents()
            Palette.load(i) foreach add
          }
        case f: IFolder if (f.getName == "META-INF") ⇒
          val file = f.getFile("palette.xml")
          if (file.exists) {
            Palette.load(file.getContents()) foreach add
          }
        case o ⇒
          println("OTHER " + o.getClass() + " " + o)
      }
    }
    // @Annotation
    search(
      patternAnnotation(classOf[Box].getName),
      projectScope(project),
      monitor)
      .flatMap(Palette.load(_)).foreach { add }
    // add ports
    Palette.portsEntries foreach add
  }
}

object Palette {
  val portsPkg = "<ports>"
  private def portToProxy(port: PortDir) = PaletteEntry(
    Name(portsPkg + "." + port.str),
    None, None, false, None, None, false)
  val InEntry = portToProxy(In)
  val OutEntry = portToProxy(Out)
  val ShiftEntry = portToProxy(Shift)
  val portsEntries = List(InEntry, OutEntry, ShiftEntry)

  def load(t: IType): Array[PaletteEntry] = {
    val classname = Name(t.getFullyQualifiedName)
    val methodEntries = for (
      m ← t.getMethods();
      if (m.getAnnotations.exists(_.getElementName() == classOf[Apply].getName))
    ) yield {
      PaletteEntry(
        classname,
        Some(m.getElementName + m.getSignature),
        None,
        Flags.isStatic(m.getFlags),
        None,
        None,
        Expressions.isTemplateExpression(classname))
    }
    if (methodEntries.isEmpty)
      Array(PaletteEntry(
        classname,
        None,
        None,
        false,
        None,
        None,
        Expressions.isTemplateExpression(classname)))
    else methodEntries

  }
  def load(file: InputStream): List[PaletteEntry] = {
    if (file == null) return List()
    else {
      try {
        val node = xml.XML.load(file)
        var entries = Buffer[PaletteEntry]()
        for (entry ← node.child) {
          if (entry.label == "entry")
            processEntry(entry).foreach { entries += _ }
        }
        entries.toList
      } catch {
        case e: Exception ⇒
          println("Error parsing palette file " + file + e)
          List()
      } finally {
        file.close
      }
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
      val muid = textOption((entry \ "@signature").text.trim)
      val name = textOption((entry \ "@name").text)
      val static = (entry \ "@static").text.trim.toLowerCase() == "true"
      val imagePath = textOption((entry \ "@image").text)
      val template = (entry \ "@template").text.trim.toLowerCase() == "true"
      val fieldsSeq = (entry \ "field").map { _.text.trim }
      val fields = if (fieldsSeq.isEmpty) None else Some(fieldsSeq.toList)
      val p = PaletteEntry(className.get, muid, fields, static, imagePath, name, template)
      Some(p)
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
case class PaletteEntry(
    className: Name,
    methodUID: Option[String],
    fields: Option[List[String]],
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
