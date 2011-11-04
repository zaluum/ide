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
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.IElementChangedListener
import org.eclipse.jdt.core.ElementChangedEvent
import org.eclipse.jdt.core.IJavaElementDelta
import org.zaluum.nide.palette.Palette
import scala.concurrent.SyncVar
import org.zaluum.nide.images.ImageMap
import org.zaluum.nide.palette.XMLPaletteLoader
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.IWorkspaceRunnable
import org.zaluum.nide.images.ImageFactory

object ZaluumProjectManager {
  private val m = collection.mutable.Map[IJavaProject, ZaluumProject]()
  object coreListener extends IElementChangedListener {
    def elementChanged(event: ElementChangedEvent) {
      processDeltaProject(event.getDelta())
    }
  }
  def processDeltaProject(delta: IJavaElementDelta) {
    delta.getElement match {
      case j: IJavaProject if (delta.getFlags & IJavaElementDelta.F_CLOSED) != 0 ⇒
        synchronized {
          if (m.contains(j)) {
            m(j).destroy()
            m -= j
          }
        }
      case _ ⇒
        for (d ← delta.getAffectedChildren())
          processDeltaProject(d)
    }
  }
  def getZaluumProject(jProject: IJavaProject) = synchronized {
    m.getOrElseUpdate(jProject, new ZaluumProject(jProject))
  }
  val exceptions = new java.util.HashSet[String]()
  exceptions.add(classOf[Shell].getName)
  exceptions.add(classOf[BoxConfigurer].getName)
  JavaCore.addElementChangedListener(coreListener)
}
class ZaluumProject private[eclipse] (val jProject: IJavaProject) {
  lazy val projectImageFactory = new ImageFactory(this)
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
  @volatile var palette: Option[Palette] = None
  @volatile var imgMap = new ImageMap(this)
  def destroy() = {
    loader.destroy()
    projectImageFactory.destroyAll()
  }
  val loader = new XMLPaletteLoader(this)
  loader.syncReload() // TODO monitor workspace run
}