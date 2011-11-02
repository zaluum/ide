package org.zaluum.nide.palette
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
import org.eclipse.jdt.core.IElementChangedListener
import org.eclipse.jdt.core.IJavaElementDelta
import org.eclipse.jdt.core.ElementChangedEvent
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.core.IJavaElementDelta._
import org.zaluum.nide.utils.Utils
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class Palette(project: IJavaProject) {
  import org.zaluum.nide.utils.JDTUtils._
  sealed trait Tree
  case class Node(name: String, child: List[Tree]) extends Tree
  case class Leaf(p: PaletteEntry) extends Tree

  @volatile var listeners = Vector[() ⇒ Unit]()
  @volatile var root: Option[Pkg] = None
  @volatile private var map: Map[String, List[PaletteEntry]] = Map();
  def get = map
  def addListener(a: () ⇒ Unit) {
    listeners = listeners :+ a
  }

  def removeListener(a: () ⇒ Unit) {
    listeners = listeners.filterNot(_ == a)
  }
  private def reload(): Unit = {
    val j = Utils.job("Update palette") { monitor ⇒
      reload(monitor)
      for (l ← listeners) { l() }
      Status.OK_STATUS
    }
    j.setPriority(Job.SHORT);
    j.schedule(); // start as soon as possible
  }
  private def reload(monitor: IProgressMonitor) {
    map = Map()
    val ord = Ordering.by((_: PaletteEntry).pkgName);
    var set = Buffer[PaletteEntry]()

      def add(e: PaletteEntry) = {
        val pkg = e.pkgName
        set.append(e)
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
    var s = set.sorted(ord).toStream
    val pkgs = ListBuffer[Pkg]()
    while (!s.isEmpty) {
      val (newpkg, next) = doPkg(s)
      pkgs ++= newpkg
      s = next
    }
    root = Some(new Pkg("", pkgs.toList, List()))
  }

  /**
   * children must be sorted by packageName
   */
  private def doPkg(meAndChildren: Stream[PaletteEntry]): (Option[Pkg], Stream[PaletteEntry]) = {
    meAndChildren match {
      case me #:: children ⇒
        var s = children
        var pkgs = List[Pkg]()
        var entries = List(me)
        while (!s.isEmpty && s.head.pkgName.startsWith(me.pkgName)) {
          if (s.head.pkgName == me.pkgName) {
            entries ::= s.head
            s = s.tail
          } else {
            val (pkg, next) = doPkg(s)
            pkgs :::= pkg.toList
            s = next
          }
        }
        (Some(Pkg(me.pkgName, pkgs.sortBy(_.name), entries.sortBy(_.simpleName))), s)
      case Stream.Empty ⇒ (None, Stream.Empty)
    }
  }

  //init
  object coreListener extends IElementChangedListener {
    def elementChanged(event: ElementChangedEvent) {
      if (project.isOpen()) {
        if (event.getType == ElementChangedEvent.POST_CHANGE) {
          val process = processDeltaSimple(event.getDelta)
          if (process)
            reload()
        }
      }
    }
  }
  JavaCore.addElementChangedListener(coreListener)
  reload()

  def processDeltaSimple(delta: IJavaElementDelta): Boolean = {
    val interestingFlags = F_ADDED_TO_CLASSPATH | F_CLASSPATH_CHANGED |
      F_ARCHIVE_CONTENT_CHANGED | F_RESOLVED_CLASSPATH_CHANGED |
      F_SUPER_TYPES | F_REORDER
    delta.getKind match {
      case IJavaElementDelta.ADDED   ⇒ true
      case IJavaElementDelta.REMOVED ⇒ true
      case IJavaElementDelta.CHANGED ⇒
        delta.getElement match {
          case cu: ICompilationUnit if (delta.getFlags & F_PRIMARY_WORKING_COPY) == 0 ⇒ true
          case _ if (delta.getFlags & interestingFlags) != 0                          ⇒ true
          case e ⇒
            val res = delta.getResourceDeltas()
            val affectedMETA = if (res != null) {
              res.exists { ird ⇒
                ird.getResource().getName == "META-INF"
              }
            } else false
            affectedMETA ||
              delta.getAffectedChildren.exists(processDeltaSimple(_))
        }
      case _ ⇒ false
    }
  }
  def destroy() {
    JavaCore.removeElementChangedListener(coreListener)
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

  def load(t: IType): Array[PaletteEntry] =
    try {
      val classname = Name(t.getFullyQualifiedName)
      val methods = t.getMethods()
      for (
        m ← methods;
        if (m.getAnnotations.exists(a ⇒
          a.getElementName == classOf[Apply].getName ||
            a.getElementName == "Apply")) // TODO model is source only? not resolved.
      ) yield {
        PaletteEntry(
          classname,
          None,
          None,
          Flags.isStatic(m.getFlags),
          None,
          None,
          Expressions.isTemplateExpression(classname))
      }
    } catch { case ex: Exception ⇒ Array() }

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
}
case class Pkg(val name: String, val child: List[Pkg], val entries: List[PaletteEntry])
case class PaletteEntry(
    className: Name,
    methodUID: Option[String],
    fields: Option[List[String]],
    static: Boolean,
    imagePath: Option[String],
    name: Option[String] = None,
    template: Boolean = false) {
  lazy val pkgName = className.packageProxy
  def simpleName = className.classNameWithoutPackage.str
  def isExpression = Expressions.find(className).isDefined
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
