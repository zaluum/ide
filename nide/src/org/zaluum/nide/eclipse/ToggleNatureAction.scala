package org.zaluum.nide.eclipse;

import java.util.Iterator;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import scala.collection.JavaConversions._

class ToggleNatureAction extends IObjectActionDelegate {

  private var selection: ISelection = _

  def run(action: IAction) {
    selection match {
      case iss: IStructuredSelection ⇒
        for (s ← iss.iterator) {
          val project = s match {
            case project: IProject ⇒ Some(project)
            case adapt: IAdaptable ⇒ Some(adapt.getAdapter(classOf[IProject]).asInstanceOf[IProject])
            case _ ⇒ None
          }
          project foreach { toggleNature(_) }
        }
    }
  }

  def selectionChanged(action: IAction, selection: ISelection) {
    this.selection = selection;
  }

  def setActivePart(action: IAction, targetPart: IWorkbenchPart) {}

  private def toggleNature(project: IProject) {
    val description = project.getDescription()
    val natures = description.getNatureIds()
    val newNatures =
      if (natures.exists(_ == ZaluumNature.NATURE_ID)) {
        natures filter { _ != ZaluumNature.NATURE_ID }
      } else {
        // Add the nature
        val buf = natures.toBuffer
        buf += ZaluumNature.NATURE_ID
        buf.toArray
      }
    description.setNatureIds(newNatures);
    project.setDescription(description, null);
  }

}
