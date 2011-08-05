package org.zaluum.nide.eclipse.launch

import org.eclipse.debug.ui.AbstractLaunchConfigurationTabGroup
import org.eclipse.debug.ui.CommonTab
import org.eclipse.debug.ui.EnvironmentTab
import org.eclipse.debug.ui.ILaunchConfigurationDialog
import org.eclipse.debug.ui.ILaunchConfigurationTab
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaArgumentsTab
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaClasspathTab
import org.eclipse.jdt.debug.ui.launchConfigurations.JavaJRETab

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
