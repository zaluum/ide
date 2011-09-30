package org.zaluum.nide.eclipse

import scala.collection.mutable.WeakHashMap
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IType
import org.zaluum.annotation.Box
import org.zaluum.nide.compiler.Expressions
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.utils.JDTUtils._
import org.eclipse.swt.widgets.Shell
import org.zaluum.basic.BoxConfigurer

object ZaluumProjectManager {
  private val m = WeakHashMap[IJavaProject, ZaluumProject]()
  def getZaluumProject(jProject: IJavaProject) = {
    m.getOrElseUpdate(jProject, new ZaluumProject(jProject))
  }
  val exceptions = new java.util.HashSet[String]()
  exceptions.add(classOf[Shell].getName)
  exceptions.add(classOf[BoxConfigurer].getName)
}
class ZaluumProject private[eclipse] (val jProject: IJavaProject) {
  lazy val imageFactory = new ImageFactory(this)

  private var _classLoader: ClassLoader = _

  def classLoader: ClassLoader = this.synchronized {
    if (_classLoader == null) refreshClassLoader
    _classLoader
  }

  def refreshClassLoader: Unit = this.synchronized {
    // Do not load swt.Shell from ProjectClassLoader.
    // Do not load BoxConfigurer from ProjectClassLoader. 
    // BoxConfigurer must pass the current Shell to the loaded class
    _classLoader = ProjectClassLoader.create(
      Thread.currentThread().getContextClassLoader(),
      jProject,
      ZaluumProjectManager.exceptions)
  }
}