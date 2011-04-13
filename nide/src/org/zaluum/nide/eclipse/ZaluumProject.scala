package org.zaluum.nide.eclipse

import org.zaluum.nide.Subject
import org.eclipse.core.resources.IResource
import org.eclipse.jdt.core.IType
import java.net.{ URL, URLClassLoader }
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.{ IJavaElement, IJavaElementDelta, IJavaProject, IClasspathEntry, ElementChangedEvent, JavaCore, IElementChangedListener }
import org.zaluum.nide.compiler._
import org.zaluum.nide.{ Cache, DoOnce }

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
class ZaluumProject(val jProject: IJavaProject) extends RootSymbol with GlobalClassPath with Subject{
  private lazy val zaluumSearcher = new ZaluumSearcher(this)
  private val typeCache = new Cache(zaluumSearcher.searchJavaType(_: Name))
  private val boxTypeCache = new Cache(zaluumSearcher.searchBoxType(_: Name))
  private def fillCache() = {
    for (b ← zaluumSearcher.searchVisibleBoxTypes) boxTypeCache.add(b.name, b)
    boxTypeCache.values
  }
  private val allBoxesOnce = new DoOnce(fillCache())(boxTypeCache.values)
  def onClassPathChanged { reset() }

  def lookupType(name: Name) = typeCache.get(name)
  def lookupBoxType(name: Name) = boxTypeCache.get(name)
  def allBoxes = allBoxesOnce.get()
  def reset() {
    typeCache.reset()
    boxTypeCache.reset()
    refreshClassLoader
    allBoxesOnce.reset()
  }
  def notifyChanged(res:IResource) {
    //if (jProject.isOnClasspath(res)) {
      reset()
      notifyObservers
    //}
  }
}

object ZaluumModelMananger {
  var map = Map[IJavaProject, ZaluumProject]()
  def rebuild {
    map = map.empty
  }
  def toJavaProject(project: IProject): Option[IJavaProject] =
    project match {
      case jp: IJavaProject ⇒ Some(jp)
      case _ ⇒ if (project.isNatureEnabled("org.eclipse.jdt.core.javanature"))
        Some(JavaCore.create(project))
      else None
    }

  def getOrCreate(p: IJavaProject): ZaluumProject =
    if (!map.contains(p)) {
      val zp = new ZaluumProject(p)
      map += (p -> zp)
      zp
    } else map(p)
  def removeProject(p: IJavaProject) {
    map -= p
  }
  def getOrCreate(p: IProject): Option[ZaluumProject] =
    toJavaProject(p) map { jp ⇒ getOrCreate(jp) }

  def notifyChanged(resource:IResource) {
    for (zp <- map.values) zp.notifyChanged(resource)
  }
  JavaCore.addElementChangedListener(new IElementChangedListener() {
    def parseDeltaElem(delta: IJavaElementDelta) {
      def isPrimaryResource = (delta.getFlags & IJavaElementDelta.F_PRIMARY_RESOURCE) != 0
      def isJavaProject = delta.getElement.getElementType == IJavaElement.JAVA_PROJECT
      def isJavaCompilationUnit = delta.getElement.getElementType == IJavaElement.COMPILATION_UNIT
      def isAdded = delta.getKind == IJavaElementDelta.ADDED
      def isRemoved = delta.getKind == IJavaElementDelta.REMOVED
      def isChanged = delta.getKind == IJavaElementDelta.CHANGED;
      def parseProjectDelta {
        val jproj = delta.getElement.asInstanceOf[IJavaProject]
        if (isAdded) getOrCreate(jproj)
        else if (isRemoved) removeProject(jproj)
        else parseChildren
      }
      def parseJavaType {
        if (isChanged) {
          val res = delta.getElement.getResource
          notifyChanged(res) 
        }
        parseChildren
      }
      def parseChildren { for (cd ← delta.getAffectedChildren) parseDeltaElem(cd) }
      // ***  
      if (isPrimaryResource && isJavaProject)
        parseProjectDelta
      else if (isPrimaryResource && isJavaCompilationUnit){
        parseJavaType
      }else
        parseChildren
    }
    def elementChanged(event: ElementChangedEvent) { parseDeltaElem(event.getDelta) }
  })

}

trait ClassPath {
  def getResource(str: String): Option[URL]
}


