package org.zaluum.nide.eclipse

import java.net.URL
import java.net.URLClassLoader
import org.eclipse.jdt.core.IJavaProject

trait GlobalClassPath extends EclipseUtils with ClassPath {
  def jProject: IJavaProject
  private def createClassLoader = {
    val urls = jProject.getResolvedClasspath(true) flatMap { e ⇒ pathToURL(e.getPath) }
    new URLClassLoader(urls, Thread.currentThread.getContextClassLoader)
  }
  private var _classLoader: ClassLoader =
    ProjectClassLoader.create(Thread.currentThread().getContextClassLoader(), jProject)
  def classLoader = _classLoader
}

trait ClassPath {
  def getResource(str: String): Option[URL]
}
