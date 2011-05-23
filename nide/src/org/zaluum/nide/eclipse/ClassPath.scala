package org.zaluum.nide.eclipse

import java.net.{URL, URLClassLoader}
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.{IJavaProject, IClasspathEntry}
import org.zaluum.nide.compiler._
import org.zaluum.nide.{Cache, DoOnce, Subject}

trait GlobalClassPath extends EclipseUtils with ClassPath {
  def jProject: IJavaProject
  private def createClassLoader = {
    val urls = jProject.getResolvedClasspath(true) flatMap { e ⇒ pathToURL(e.getPath) }
    new URLClassLoader(urls, Thread.currentThread.getContextClassLoader)
  }
  private var _classLoader: ClassLoader = createClassLoader
  def classLoader = _classLoader
  private[eclipse] def refreshClassLoader { _classLoader = createClassLoader }
}

trait ClassPath {
  def getResource(str: String): Option[URL]
}
