package org.zaluum.nide.eclipse;

import org.eclipse.core.runtime.CoreException
import org.eclipse.core.resources.{ IProjectNature, IProjectDescription, IProject, ICommand }
import org.eclipse.jdt.core.JavaCore
object ZaluumNature {
  val NATURE_ID = "org.zaluum.nide.zaluumNature";
   def hasZaluumNature(project:IProject) = {
    try {
      project.hasNature(NATURE_ID);
    } catch {case e: CoreException => false}
  }
  def addNature (project:IProject) {
    val description = project.getDescription()
    val natures = description.getNatureIds()
    if (project.hasNature(JavaCore.NATURE_ID) && !ZaluumNature.hasZaluumNature(project)) {
      val buf = natures.toBuffer
      buf.prepend(ZaluumNature.NATURE_ID)
      description.setNatureIds(buf.toArray);
      project.setDescription(description, null);
    }
  }
  def removeNature(project:IProject) {
    val description = project.getDescription()
    val natures = description.getNatureIds()
    description.setNatureIds(natures filter { _ != ZaluumNature.NATURE_ID });
    project.setDescription(description, null);
    
  }
  def toggleNature(project: IProject) {
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
