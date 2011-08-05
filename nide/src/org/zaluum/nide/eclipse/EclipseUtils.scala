package org.zaluum.nide.eclipse

import java.io.IOException
import java.net.URL

import scala.collection.mutable.Buffer
import scala.util.control.Exception.catching

import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IResourceVisitor
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.internal.ui.infoviews.JavadocView
import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jface.internal.text.html.BrowserInput
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.compiler.Name
object EclipseUtils {
  def pathToResource(path: IPath) = {
    Option(ResourcesPlugin.getWorkspace.getRoot.findMember(path))
  }
  def withProgress[A >: Null](name: String)(body: IProgressMonitor ⇒ A): A = {
    var a: A = null
    val ps = PlatformUI.getWorkbench().getProgressService
    val run = new IRunnableWithProgress() {
      def run(monitor: IProgressMonitor) {
        monitor.setTaskName(name)
        a = body(monitor)
        monitor.done;
      }
    }
    ps.busyCursorWhile(run)
    a
  }
  def async(display: Display)(body: ⇒ Unit) {
    display.asyncExec(new Runnable() {
      def run {
        body
      }
    })
  }
  def async(body: ⇒ Unit) {
    async(Display.getCurrent)(body _)
  }
}
trait EclipseUtils {
  def jProject: IJavaProject
  def project = jProject.getProject
  def allSourceZaluums: Seq[IFile] = {
    val b = Buffer[IFile]()
    project.accept(
      new IResourceVisitor {
        def visit(resource: IResource) = resource match {
          case f: IFile if ("zaluum" == f.getFileExtension && isSource(f)) ⇒
            b += f
            false
          case c: IContainer ⇒ true
          case _             ⇒ true
        }
      })
    b.toSeq
  }
  def extractPackageName(res: IResource): Option[String] = {
    sourcePaths find { _.isPrefixOf(res.getFullPath) } map { sp ⇒
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
        case None              ⇒ result
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
        case _     ⇒ pathToURL(path.append(new Path(file)))
      }
  }
  def getResource(str: String): Option[URL] = {
    val cpaths = jProject.getResolvedClasspath(true)
    cpaths.view.flatMap { cp ⇒ pathFileToURL(cp.getPath, str) } headOption
  }
  def forceViewJavaDoc(i: IType) {
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