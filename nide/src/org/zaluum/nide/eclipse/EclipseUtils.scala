package org.zaluum.nide.eclipse

import org.zaluum.nide.newcompiler.Name
import org.eclipse.core.runtime.Path
import java.net.URL
import org.eclipse.core.runtime.IPath
import org.eclipse.core.resources.{ IFile, IResourceVisitor, IResource, IContainer }
import org.eclipse.jdt.core.{ IJavaProject, IClasspathEntry }
import scala.util.control.Exception._

trait EclipseUtils {
  def jproject: IJavaProject
  def project = jproject.getProject
  def visitSourceZaluums(body: IFile ⇒ Unit) {
    project.accept(
      new IResourceVisitor {
        def visit(resource: IResource) = resource match {
          case f: IFile if ("zaluum" == f.getFileExtension && isSource(f)) ⇒
            body(f)
            false
          case c: IContainer ⇒ true
          case _ ⇒ true
        }
      })
  }
  def isSourceCP(e: IClasspathEntry) = e.getEntryKind == IClasspathEntry.CPE_SOURCE
  def sourcePaths = jproject.getResolvedClasspath(true) collect { case p if isSourceCP(p) ⇒ p.getPath }
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
    val cpaths = jproject.getResolvedClasspath(true)
    cpaths.view.flatMap { cp ⇒ pathFileToURL(cp.getPath, str) } headOption
  }
}