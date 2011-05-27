package org.zaluum.nide.eclipse.launch

import org.eclipse.debug.ui.{ ILaunchConfigurationTab, CommonTab, EnvironmentTab, ILaunchConfigurationDialog, AbstractLaunchConfigurationTabGroup }
import org.eclipse.jdt.debug.ui.launchConfigurations.{ JavaClasspathTab, JavaJRETab, JavaArgumentsTab }

class ZaluumLaunchConfigurationTabGroup extends AbstractLaunchConfigurationTabGroup {

  def createTabs(dialog: ILaunchConfigurationDialog, mode: String) {
    val tabs = Array[ILaunchConfigurationTab](
      new ZaluumLauncherTab(),
      new JavaArgumentsTab(),
      new JavaJRETab(),
      new JavaClasspathTab(),
      new EnvironmentTab(),
      new CommonTab());
    setTabs(tabs);
  }

}
