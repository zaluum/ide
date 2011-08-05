package org.zaluum.nide.eclipse;
import scala.collection.JavaConversions.asScalaIterator

import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.jface.action.IAction
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.ui.IObjectActionDelegate
import org.eclipse.ui.IWorkbenchPart

abstract class ProjectAction extends IObjectActionDelegate {

  private var selection: ISelection = _

  def run(action: IAction) {
    selection match {
      case iss: IStructuredSelection ⇒
        for (s ← iss.iterator) {
          val project = s match {
            case project: IProject ⇒ Some(project)
            case adapt: IAdaptable ⇒ Some(adapt.getAdapter(classOf[IProject]).asInstanceOf[IProject])
            case _                 ⇒ None
          }
          project foreach { projectAction(_) }
        }
    }
  }

  def selectionChanged(action: IAction, selection: ISelection) {
    this.selection = selection;
  }

  def setActivePart(action: IAction, targetPart: IWorkbenchPart) {}
  protected def projectAction(p: IProject)
}
class RemoveZaluumAction extends ProjectAction {
  override protected def projectAction(p: IProject) {
    ZaluumNature.removeNature(p)
    p.build(IncrementalProjectBuilder.FULL_BUILD, null)
    p.refreshLocal(IResource.DEPTH_INFINITE, null)
  }
}
class AddZaluumAction extends ProjectAction {
  override protected def projectAction(p: IProject) {
    ZaluumNature.addNature(p)
    p.build(IncrementalProjectBuilder.FULL_BUILD, null)
    p.refreshLocal(IResource.DEPTH_INFINITE, null)
  }
}