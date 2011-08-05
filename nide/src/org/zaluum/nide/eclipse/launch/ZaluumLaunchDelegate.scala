package org.zaluum.nide.eclipse.launch

import java.text.MessageFormat

import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.variables.VariablesPlugin
import org.eclipse.debug.core.ILaunch
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.jdt.internal.launching.LaunchingMessages
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate
import org.eclipse.jdt.launching.ExecutionArguments
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants
import org.eclipse.jdt.launching.VMRunnerConfiguration
import org.zaluum.launch.Run
object ZaluumLaunchDelegate {

}
class ZaluumLaunchDelegate extends AbstractJavaLaunchConfigurationDelegate {
  def verifyMainBoxName(configuration: ILaunchConfiguration) = {
    val name = getMainBoxName(configuration);
    if (name == null) {
      abort("Main box not specified", null,
        IJavaLaunchConfigurationConstants.ERR_UNSPECIFIED_MAIN_TYPE);
    }
    name;
  }
  def getMainBoxName(configuration: ILaunchConfiguration): String = {
    val mainType = configuration.getAttribute(
      ZaluumLauncherTab.ATTR_MAIN_BOX, null.asInstanceOf[String]);
    if (mainType != null)
      VariablesPlugin.getDefault().getStringVariableManager()
        .performStringSubstitution(mainType);
    else null;
  }
  def launch(configuration: ILaunchConfiguration, mode: String, launch: ILaunch, monitorO: IProgressMonitor) {
    val monitor = if (monitorO == null) new NullProgressMonitor() else monitorO
    monitor.beginTask(MessageFormat.format("{0}...", Array(configuration.getName)), 3); //$NON-NLS-1$
    if (monitor.isCanceled())
      return ;
    try {
      monitor.subTask(LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Verifying_launch_attributes____1);
      // ZALUUM
      val mainTypeName = classOf[org.zaluum.launch.Run].getName
      // ZALUUM

      val runner = getVMRunner(configuration, mode);

      val workingDir = verifyWorkingDirectory(configuration);
      var workingDirName: String = null;
      if (workingDir != null) {
        workingDirName = workingDir.getAbsolutePath();
      }

      // Environment variables
      val envp = getEnvironment(configuration);

      // Program & VM arguments
      // ZALUUM
      val boxName = verifyMainBoxName(configuration)
      val pgmArgs = boxName + " " + getProgramArguments(configuration);
      // ZALUUM
      val vmArgs = getVMArguments(configuration);
      val execArgs = new ExecutionArguments(vmArgs, pgmArgs);

      // VM-specific attributes
      val vmAttributesMap = getVMSpecificAttributesMap(configuration);

      // Classpath
      val classpath = getClasspath(configuration);

      // Create VM config
      val runConfig = new VMRunnerConfiguration(mainTypeName, classpath);
      runConfig.setProgramArguments(execArgs.getProgramArgumentsArray());
      runConfig.setEnvironment(envp);
      runConfig.setVMArguments(execArgs.getVMArgumentsArray());
      runConfig.setWorkingDirectory(workingDirName);
      runConfig.setVMSpecificAttributesMap(vmAttributesMap);

      // Bootpath
      runConfig.setBootClassPath(getBootpath(configuration));

      // check for cancellation
      if (monitor.isCanceled()) {
        return ;
      }

      // stop in main
      prepareStopInMain(configuration);

      // done the verification phase
      monitor.worked(1);

      monitor.subTask(LaunchingMessages.JavaLocalApplicationLaunchConfigurationDelegate_Creating_source_locator____2);
      // set the default source locator if required
      setDefaultSourceLocator(launch, configuration);
      monitor.worked(1);

      // Launch the configuration - 1 unit of work
      runner.run(runConfig, launch, monitor);

      // check for cancellation
      if (monitor.isCanceled())
        return ;
    } finally {
      monitor.done();
    }
  }

}