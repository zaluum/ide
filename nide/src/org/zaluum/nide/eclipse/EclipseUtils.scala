package org.zaluum.nide.eclipse

import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.internal.ui.JavaPlugin
import scala.collection.mutable.Buffer
import org.zaluum.nide.compiler.Name
import org.eclipse.core.runtime.Path
import java.net.URL
import org.eclipse.core.runtime.IPath
import org.eclipse.core.resources.{ IFile, IResourceVisitor, IResource, IContainer }
import org.eclipse.jdt.core.{ IJavaProject, IClasspathEntry }
import scala.util.control.Exception._
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
object EclipseUtils {
  def pathToResource(path:IPath) = {
    Option(ResourcesPlugin.getWorkspace.getRoot.findMember(path))
  }
}
trait EclipseUtils {
  def jProject: IJavaProject
  def project = jProject.getProject
  def allSourceZaluums : Seq[IFile] = {
    val b= Buffer[IFile]()
    project.accept(
      new IResourceVisitor {
        def visit(resource: IResource) = resource match {
          case f: IFile if ("zaluum" == f.getFileExtension && isSource(f)) ⇒
            b+=f
            false
          case c: IContainer ⇒ true
          case _ ⇒ true
        }
      })
    b.toSeq
  }
  def extractPackageName(res:IResource) : Option[String] = {
    sourcePaths find { _.isPrefixOf(res.getFullPath) } map { sp =>
      val path = res.getFullPath
      val pkgPath = path.removeFirstSegments(sp.segmentCount).removeFileExtension.removeLastSegments(1)
      pkgPath.segments.mkString(".")
    }
  }
  def isSourceCP(e: IClasspathEntry) = e.getEntryKind == IClasspathEntry.CPE_SOURCE
  def sourcePaths = jProject.getResolvedClasspath(true) collect { case p if isSourceCP(p) ⇒ p.getPath }
  def isSource(file: IFile) = { sourcePaths exists (p ⇒ p.isPrefixOf(file.getFullPath)) }
  def toClassName(f: IFile): Option[Name] = {
    val path = f.getFullPath
    val oSourcePath = sourcePaths.find(_.isPrefixOf(path))
    oSourcePath map { sourcePath ⇒
      val relativePath = path.removeFirstSegments(sourcePath.segmentCount)
      val result = relativePath.segments.reduceLeft(_ + "." + _)
      Option(relativePath.getFileExtension) match {
        case Some(str: String) ⇒ result.dropRight(str.length + 1)
        case None ⇒ result
      }
    } map { Name(_) }
  }
  def pathToURL(path: IPath): Option[URL] = {
    Option(workspaceRoot.findMember(path)) map { p ⇒
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
  def workspaceRoot = project.getWorkspace.getRoot
  def pathFileToURL(path: IPath, file: String) = file match {
    case "" ⇒ None
    case _ ⇒
      path.getFileExtension match {
        case "jar" ⇒ jarURL(path, file)
        case _ ⇒ pathToURL(path.append(new Path(file)))
      }
  }
  def getResource(str: String): Option[URL] = {
    val cpaths = jProject.getResolvedClasspath(true)
    cpaths.view.flatMap { cp ⇒ pathFileToURL(cp.getPath, str) } headOption
  }
  def forceViewJavaDoc(i:IType) {
    import org.eclipse.jdt.internal.ui.infoviews.JavadocView
    import org.eclipse.jface.internal.text.html.BrowserInput;
    // force JavaDoc
    val in = new BrowserInput(null) {
      def getInputElement = i
      def getInputName = i.getElementName
    }
    val jview = JavaPlugin.getActivePage().findView(JavaUI.ID_JAVADOC_VIEW).asInstanceOf[JavadocView];
    if (jview != null)
      jview.setInput(in)
  }
}