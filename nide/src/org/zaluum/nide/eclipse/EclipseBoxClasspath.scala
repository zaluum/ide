package org.zaluum.nide.eclipse
import java.net.URL
import org.eclipse.core.resources.{ IProject, IFile, IResource }
import org.eclipse.core.runtime.{ Path, IPath }
import org.eclipse.jdt.core.{ IJavaElement, IType, IAnnotatable, IJavaProject, IAnnotation, IMemberValuePair, IClasspathEntry }
import org.eclipse.jdt.core.search.{ SearchEngine, SearchPattern, SearchRequestor, SearchMatch, IJavaSearchConstants, TypeReferenceMatch }
import org.eclipse.jdt.internal.core.JavaModelManager
import org.zaluum.nide.compiler.ScannedBoxClassPath
import org.zaluum.nide.model.{ BoxClass, TypedPort, Point, ProtoModel }
import scala.util.control.Exception._

class EclipseBoxClasspath(project: IProject) extends ScannedBoxClassPath with EclipseUtils {
  var cache = Map[String, BoxClass]()
  def jmodel = JavaModelManager.getJavaModelManager.getJavaModel
  def jproject = jmodel.getJavaProject(project);

  def update() {
    cache = cache.empty
    val searchScope = SearchEngine.createJavaSearchScope(
      Array[IJavaElement](jproject))
    val search = new SearchEngine()
    val pattern = SearchPattern.createPattern("org.zaluum.nide.java.Box",
      IJavaSearchConstants.ANNOTATION_TYPE,
      IJavaSearchConstants.ANNOTATION_TYPE_REFERENCE,
      SearchPattern.R_EXACT_MATCH)
    val participants = Array(SearchEngine.getDefaultSearchParticipant())

    def isAnnType(t: IType, a: IAnnotation, str: String) = {
      val names = t.resolveType(a.getElementName)
      if (names == null) {
        false
      } else {
        names exists { posible ⇒
          val fullname = posible(0) + "." + posible(1)
          fullname == str
        }
      }
    }
    def findAnnotations(t: IType, ann: IAnnotatable, str: String) = ann.getAnnotations find { a ⇒ isAnnType(t, a, str) }

    def findStringValueOfAnnotation(a: IAnnotation, key: String) = {
      a.getMemberValuePairs find { v ⇒
        v.getMemberName == key && v.getValueKind == IMemberValuePair.K_STRING
      } map { _.getValue.asInstanceOf[String] }
    }
    def findIntegerValueOfAnnotation(a: IAnnotation, key: String) = {
      a.getMemberValuePairs find { v ⇒
        v.getMemberName == key && v.getValueKind == IMemberValuePair.K_INT
      } map { _.getValue.asInstanceOf[Int] }
    }
    def processType(t: IType) {
      val fqn = t.getFullyQualifiedName;
      val img = findAnnotations(t, t, "org.zaluum.nide.java.BoxImage").headOption flatMap { a ⇒
        findStringValueOfAnnotation(a, "value")
      }

      val bc = new BoxClass(fqn, false, img.getOrElse(""))
      def pointOf(a: IAnnotation) = {
        val ox = findIntegerValueOfAnnotation(a, "x")
        val oy = findIntegerValueOfAnnotation(a, "y")
        (ox, oy) match {
          case (Some(x), Some(y)) ⇒ Point(x, y)
          case _ ⇒ Point(0, 0)
        }
      }
      for (f ← t.getFields) {
        findAnnotations(t, f, "org.zaluum.nide.java.In") foreach { a ⇒
          bc.ports += TypedPort(f.getTypeSignature, true, f.getElementName, pointOf(a)) // FIXME 
        }
        findAnnotations(t, f, "org.zaluum.nide.java.Out") foreach { a ⇒
          bc.ports += TypedPort(f.getTypeSignature, false, f.getElementName, pointOf(a)) // FIXME 
        }
      }
      cache += (bc.className -> bc)
    }
    val searchRequestor = new SearchRequestor() {
      def acceptSearchMatch(matchh: SearchMatch) {
        matchh match {
          case t: TypeReferenceMatch ⇒
            t.getElement match {
              case t: IType ⇒ processType(t)
              case other ⇒ println("ohter match " + other + " " + other.getClass)
            }
        }
      }
    }
    // WORK
    search.search(pattern, participants, searchScope, searchRequestor, null)
    // FIND ZALUUMS IN SOURCE
    visitSourceZaluums { f ⇒
      toClassName(f) foreach { name ⇒
        cache += (name -> ProtoModel.readDefinition(f.getContents, name))
      }
    }
  }

  def boxClasses: Set[BoxClass] = { cache.values.toSet }
  def find(str: String): Option[BoxClass] = cache.get(str)

  def toClassName(f: IFile) = {
    val path = f.getFullPath
    val oSourcePath = sourcePaths.find(_.isPrefixOf(path))
    oSourcePath map { sourcePath ⇒
      val relativePath = path.removeFirstSegments(sourcePath.segmentCount)
      val result = relativePath.segments.reduceLeft(_ + "." + _)
      Option(relativePath.getFileExtension) match {
        case Some(str: String) ⇒ result.dropRight(str.length + 1)
        case None ⇒ result
      }
    }
  }
  def pathToURL(path: IPath): Option[URL] = {
    Option(root.findMember(path)) map { p ⇒
      p.getLocationURI.toURL
    } orElse {
      val f = path.toFile;
      if (f.exists) Some(f.toURI.toURL) else None
    }
  }

  def jarURL(jarPath: IPath, filePath: String): Option[URL] = filePath match {
    case "" ⇒ None
    case _ ⇒
      pathToURL(jarPath) flatMap { jarURL ⇒
        val url = new URL("jar:" + jarURL + "!/" + filePath)
        catching(classOf[java.io.IOException]) opt {
          val c = url.openConnection.asInstanceOf[java.net.JarURLConnection]
          c.connect()
          url
        }
      }
  }
  def root = project.getWorkspace.getRoot
  def pathFileToURL(path: IPath, file: String) = file match {
    case "" ⇒ None
    case _ ⇒
      path.getFileExtension match {
        case "jar" ⇒ jarURL(path, file)
        case _ ⇒ pathToURL(path.append(new Path(file)))
      }
  }
  def getResource(str: String): Option[URL] = {
    val cpaths = jproject.getResolvedClasspath(true)
    cpaths.view.flatMap { cp ⇒ pathFileToURL(cp.getPath, str) } headOption
  }
}