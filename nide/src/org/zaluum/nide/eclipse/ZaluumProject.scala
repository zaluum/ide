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
class ZaluumProject(val jProject: IJavaProject) extends RootSymbol with GlobalClassPath with Subject{
  private lazy val zaluumSearcher = new ZaluumLoader(this)
  private val typeCache = new Cache(zaluumSearcher.searchJavaType(_: Name))
  private val boxTypeCache = new Cache(zaluumSearcher.searchBoxType(_: Name))
  
  def lookupType(name: Name) = typeCache.get(name)
  def lookupBoxType(name: Name) = boxTypeCache.get(name)
  def alreadyDefinedBoxType(name:Name) = false
  def index = zaluumSearcher.index
  def reset() {
    typeCache.reset()
    boxTypeCache.reset()
    refreshClassLoader
  }
  def onChanged(res:IResource) {
    if (jProject.isOnClasspath(res)) {
      //TODO refine the reset and notify should only affect controller that depend on changed resources
      reset()
      notifyObservers
    }
  }
}
