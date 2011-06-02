package org.zaluum.nide.eclipse;

import org.eclipse.core.runtime.CoreException
import org.eclipse.core.resources.{ IProjectNature, IProjectDescription, IProject, ICommand }
import org.eclipse.jdt.core.JavaCore
import scala.collection.mutable.ArrayBuffer
import org.zaluum.nide.Activator
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.launching.JavaRuntime
object ZaluumNature {
  val NATURE_ID = "org.zaluum.nide.zaluumNature";
  def hasZaluumNature(project: IProject) = {
    try {
      project.hasNature(NATURE_ID);
    } catch { case e: CoreException ⇒ false }
  }
  def addNature(project: IProject) {
    val description = project.getDescription()
    val natures = description.getNatureIds()
    if (project.hasNature(JavaCore.NATURE_ID) && !ZaluumNature.hasZaluumNature(project)) {
      val buf = natures.toBuffer
      buf.prepend(ZaluumNature.NATURE_ID)
      description.setNatureIds(buf.toArray);
      project.setDescription(description, null);
    }

  }
  def removeNature(project: IProject) {
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
  protected def plugin = Activator.plugin
  private var project: IProject = _
  def configure() {
    if (project == null || !project.isOpen)
      return

    plugin.check {
      val jp = JavaCore.create(getProject)
      removeClasspathContainer(jp)
      val buf = ArrayBuffer(jp.getRawClasspath: _*)

      val zaluumLibEntry = JavaCore.newContainerEntry(Path.fromPortableString(plugin.zaluumLibId))
      if (!buf.exists(_.getPath.toPortableString.startsWith(JavaRuntime.JRE_CONTAINER)))
        buf += JavaCore.newContainerEntry(Path.fromPortableString(JavaRuntime.JRE_CONTAINER))

      buf += zaluumLibEntry
      jp.setRawClasspath(buf.toArray, null)
      jp.save(null, true)
    }
  }
  private def removeClasspathContainer(jp: IJavaProject) {
    val scalaLibPath = Path.fromPortableString(plugin.zaluumLibId)
    val buf = jp.getRawClasspath filter { _.getPath != scalaLibPath }
    jp.setRawClasspath(buf, null)
  }
  def deconfigure() {
    if (project == null || !project.isOpen)
      return

    val jp = JavaCore.create(getProject)
    removeClasspathContainer(jp)
    jp.save(null, true)
  }

  def getProject() = project
  def setProject(project: IProject) {
    this.project = project;
  }

}
