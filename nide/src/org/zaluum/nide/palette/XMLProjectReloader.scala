package org.zaluum.nide.palette
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.jdt.core.IJavaElementDelta.F_ADDED_TO_CLASSPATH
import org.eclipse.jdt.core.IJavaElementDelta.F_ARCHIVE_CONTENT_CHANGED
import org.eclipse.jdt.core.IJavaElementDelta.F_CLASSPATH_CHANGED
import org.eclipse.jdt.core.IJavaElementDelta.F_PRIMARY_WORKING_COPY
import org.eclipse.jdt.core.IJavaElementDelta.F_REORDER
import org.eclipse.jdt.core.IJavaElementDelta.F_RESOLVED_CLASSPATH_CHANGED
import org.eclipse.jdt.core.IJavaElementDelta.F_SUPER_TYPES
import org.eclipse.jdt.core.ElementChangedEvent
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.core.IElementChangedListener
import org.eclipse.jdt.core.IJavaElementDelta
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.JavaCore
import org.zaluum.nide.utils.Utils

abstract class XMLProjectReloader(project: IJavaProject) {
  @volatile var listeners = Vector[() ⇒ Unit]()
  def addListener(a: () ⇒ Unit) {
    listeners = listeners :+ a
  }

  def removeListener(a: () ⇒ Unit) {
    listeners = listeners.filterNot(_ == a)
  }
  protected def reload(p: IProgressMonitor)
  def asyncReload(): Job = {
    val j = Utils.job("Updating palette") { monitor ⇒
      syncReload(monitor)
      Status.OK_STATUS
    }
    j.setPriority(Job.SHORT);
    j.schedule(); // start as soon as possible
    j
  }
  def syncReload(monitor: IProgressMonitor = null) {
    reload(null)
    for (l ← listeners) { l() }
  }
  //init
  object coreListener extends IElementChangedListener {
    def elementChanged(event: ElementChangedEvent) {
      if (project.isOpen()) {
        if (event.getType == ElementChangedEvent.POST_CHANGE) {
          val process = processDeltaSimple(event.getDelta)
          if (process)
            asyncReload()
        }
      }
    }
  }
  JavaCore.addElementChangedListener(coreListener)

  def processDeltaSimple(delta: IJavaElementDelta): Boolean = {
    val interestingFlags = F_ADDED_TO_CLASSPATH | F_CLASSPATH_CHANGED |
      F_ARCHIVE_CONTENT_CHANGED | F_RESOLVED_CLASSPATH_CHANGED |
      F_SUPER_TYPES | F_REORDER
    delta.getKind match {
      case IJavaElementDelta.ADDED   ⇒ true
      case IJavaElementDelta.REMOVED ⇒ true
      case IJavaElementDelta.CHANGED ⇒
        delta.getElement match {
          case cu: ICompilationUnit if (delta.getFlags & F_PRIMARY_WORKING_COPY) == 0 ⇒ true
          case _ if (delta.getFlags & interestingFlags) != 0                          ⇒ true
          case e ⇒
            val res = delta.getResourceDeltas()
            val affectedMETA = if (res != null) {
              res.exists { ird ⇒
                ird.getResource().getName == "META-INF"
              }
            } else false
            affectedMETA ||
              delta.getAffectedChildren.exists(processDeltaSimple(_))
        }
      case _ ⇒ false
    }
  }
  def destroy() {
    JavaCore.removeElementChangedListener(coreListener)
  }
}