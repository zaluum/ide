package org.zaluum.nide.eclipse

import org.eclipse.core.resources.{IProject, IResource}
import org.eclipse.jdt.core.{IJavaElement, IJavaElementDelta, IJavaProject, ElementChangedEvent, JavaCore, IElementChangedListener}

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
    for (zp <- map.values) zp.onChanged(resource)
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
        if (isChanged || isRemoved) {
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




