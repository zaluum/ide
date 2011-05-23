package org.zaluum.nide.eclipse;

import org.eclipse.core.runtime.CoreException
import org.eclipse.core.resources.{ IProjectNature, IProjectDescription, IProject, ICommand }
object ZaluumNature {
  val NATURE_ID = "org.zaluum.nide.zaluumNature";
   def hasZaluumNature(project:IProject) = {
    try {
      project.hasNature(NATURE_ID);
    } catch {case e: CoreException => false}
  }
}
class ZaluumNature extends IProjectNature {

  private var project: IProject = _
  def configure() {
  }

  def deconfigure() {
  }

  def getProject() = project
  def setProject(project: IProject) {
    this.project = project;
  }
 

}
