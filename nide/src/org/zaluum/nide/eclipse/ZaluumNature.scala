package org.zaluum.nide.eclipse;
import org.eclipse.core.resources.{ IProjectNature, IProjectDescription, IProject, ICommand }
object ZaluumNature {
  val NATURE_ID = "org.zaluum.nide.zaluumNature";
}
class ZaluumNature extends IProjectNature {

  private var project: IProject = _
  def configure() {
    val description = project.getDescription
    val commands = description.getBuildSpec

    if (commands.exists(_.getBuilderName == ZaluumBuilder.BUILDER_ID))
      return ;
    val newCommands = commands.toBuffer
    val command = description.newCommand();
    command.setBuilderName(ZaluumBuilder.BUILDER_ID);
    newCommands += command
    description.setBuildSpec(newCommands.toArray);
    project.setDescription(description, null);
  }

  def deconfigure() {
    val description = project.getDescription
    val commands = description.getBuildSpec

    val filtered = commands.filter { _.getBuilderName != ZaluumBuilder.BUILDER_ID }
    description.setBuildSpec(filtered)
    project.setDescription(description, null)
  }

  def getProject() = project
  def setProject(project: IProject) {
    this.project = project;
  }

}
