package org.zaluum.nide.palette
import java.io.InputStream
import scala.Option.option2Iterable
import scala.collection.mutable.Buffer
import scala.collection.mutable.ListBuffer
import scala.collection.Map
import org.eclipse.core.resources.IFolder
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.IJarEntryResource
import org.zaluum.annotation.Box
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.utils.JDTUtils.patternAnnotation
import org.zaluum.nide.utils.JDTUtils.projectScope
import org.zaluum.nide.utils.JDTUtils.search
import org.zaluum.nide.images.ImageMap

class XMLPaletteLoader(project: ZaluumProject) extends XMLProjectReloader(project.jProject) {
  protected def withXMLFiles(func: InputStream ⇒ Unit) {
    for (
      r ← project.jProject.getAllPackageFragmentRoots;
      res ← r.getNonJavaResources
    ) {
      try {
        res match {
          case r: IJarEntryResource if r.getName.endsWith("META-INF") ⇒
            r.getChildren().find(_.getName() == "zaluum.xml").foreach { f ⇒
              func(f.getContents())
            }
          case f: IFolder if (f.getName == "META-INF") ⇒
            val file = f.getFile("zaluum.xml")
            if (file.exists) {
              func(file.getContents(true))
            }
          case o ⇒
            println("OTHER " + o.getClass() + " " + o)
        }
      } catch {
        case e: Exception ⇒ e.printStackTrace() // FIXME log
      }
    }
  }
  protected def reload(monitor: IProgressMonitor) {
    var map = Map[String, List[PaletteEntry]]()
    val ord = Ordering.by((_: PaletteEntry).pkg);
    var set = Buffer[PaletteEntry]()
    val img = new ImageMap(project)
      def add(e: PaletteEntry) = {
        val pkg = e.pkg
        set.append(e)
        map.get(pkg) match {
          case Some(l) ⇒ map += (pkg -> (e :: l))
          case None    ⇒ map += (pkg -> List(e))
        }
      }
    withXMLFiles { i ⇒
      val (le, li) = XMLParser.load(i)
      le foreach add
      li foreach { case (k, v) ⇒ img.add(k, v) }
    }
    search(
      patternAnnotation(classOf[Box].getName),
      projectScope(project.jProject),
      monitor)
      .flatMap(PaletteEntry.load(_)).foreach { add }
    // add ports
    PaletteEntry.portsEntries foreach add
    var s = set.sorted(ord).toStream
    val pkgs = ListBuffer[Pkg]()
    while (!s.isEmpty) {
      val (newpkg, next) = doPkg(s)
      pkgs ++= newpkg
      s = next
    }
    val root = new Pkg("", pkgs.toList, List())
    project.palette = Some(new Palette(root, map))
    project.imgMap = img
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
        while (!s.isEmpty && s.head.pkg.startsWith(me.pkg)) {
          if (s.head.pkg == me.pkg) {
            entries ::= s.head
            s = s.tail
          } else {
            val (pkg, next) = doPkg(s)
            pkgs :::= pkg.toList
            s = next
          }
        }
        (Some(Pkg(me.pkg, pkgs.sortBy(_.name), entries.sortBy(_.name))), s)
      case Stream.Empty ⇒ (None, Stream.Empty)
    }
  }

}