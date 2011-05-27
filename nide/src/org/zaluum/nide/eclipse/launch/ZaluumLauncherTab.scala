package org.zaluum.nide.eclipse.launch
import java.text.MessageFormat
import org.eclipse.core.resources.{IResource, ResourcesPlugin}
import org.eclipse.core.runtime.CoreException
import org.eclipse.debug.core.{ILaunchConfiguration, ILaunchConfigurationWorkingCopy}
import org.eclipse.debug.internal.ui.SWTFactory
import org.eclipse.debug.ui.ILaunchConfigurationDialog
import org.eclipse.jdt.core.{IClassFile, ICompilationUnit, IMember, IJavaElement, IJavaProject, JavaModelException, IType}
import org.eclipse.jdt.internal.debug.ui.actions.ControlAccessibleListener
import org.eclipse.jdt.internal.debug.ui.launcher.{DebugTypeSelectionDialog, LauncherMessages, AbstractJavaMainTab}
import org.eclipse.jdt.internal.debug.ui.IJavaDebugHelpContextIds
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants
import org.eclipse.jdt.ui.{ISharedImages, JavaUI}
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.jface.window.Window
import org.eclipse.swt.layout.{GridData, GridLayout}
import org.eclipse.swt.widgets.{Text, Composite}
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.eclipse.SearchUtils
import org.zaluum.nide.zge.SWTScala
import org.zaluum.nide.Activator
object ZaluumLauncherTab {
  val ATTR_MAIN_BOX = Activator.PLUGIN_ID + ".MAIN_BOX"
}
class ZaluumLauncherTab extends AbstractJavaMainTab {
  var mainText: Text = _
  override def getName = "Zaluum Application"
  override def getImage = JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_CLASS);
  override def createControl(parent: Composite) {
    val comp = SWTFactory.createComposite(parent, parent.getFont(), 1, 1, GridData.FILL_BOTH);
    comp.getLayout.asInstanceOf[GridLayout].verticalSpacing = 0;
    createProjectEditor(comp);
    createVerticalSpacer(comp, 1);
    createMainBoxEditor(comp);
    setControl(comp);
    PlatformUI.getWorkbench().getHelpSystem().setHelp(getControl(), IJavaDebugHelpContextIds.LAUNCH_CONFIGURATION_DIALOG_MAIN_TAB);

  }
  import SWTScala._
  protected def createMainBoxEditor(parent: Composite) {
    val txt = "Main box:"
    val group = SWTFactory.createGroup(parent, txt, 2, 1, GridData.FILL_HORIZONTAL);
    mainText = SWTFactory.createSingleText(group, 1);
    addModifyReaction(mainText) {
      updateLaunchConfigurationDialog();
    }
    ControlAccessibleListener.addListener(mainText, txt);

    val fSearchButton = createPushButton(group, "Search...", null);
    addReaction(fSearchButton) {
      handleSearchButtonSelected();
    }
  }
  protected def handleSearchButtonSelected() {
    val javaProject = getJavaProject();
    try {
      val types = findAllRunnableTypes(javaProject);
      if (types.length == 0) {
        MessageDialog.openWarning(getShell(), "No Zaluum classes to run",
          "There are no compiled Zaluum classes to run in this project");
        return ;
      }
      val mmsd = new DebugTypeSelectionDialog(getShell(), types.toArray, "Select Main Zaluum Class"); 
      if (mmsd.open() == Window.CANCEL) {
        return;
      }
      val results = mmsd.getResult();  
      val tpe = results(0).asInstanceOf[IType];
      if (tpe != null) {
        mainText.setText(tpe.getFullyQualifiedName());
        fProjText.setText(tpe.getJavaProject().getElementName());
      }
    } catch {
      case e: JavaModelException ⇒
        Activator.logError("Exception when launching " + javaProject, e);
    }
  }
  protected def findAllRunnableTypes(javaProject: IJavaProject): List[IType] = {
    import SearchUtils._
    search(patternAnnotation(classOf[org.zaluum.annotation.Box].getName), sourcesScope(javaProject), null)
  }
  override def initializeFrom(config: ILaunchConfiguration) {
    super.initializeFrom(config);
    updateMainBoxFromConfig(config);
  }
  def updateMainBoxFromConfig(config: ILaunchConfiguration) {
    import scala.util.control.Exception._
    val mainTypeName = failAsValue(classOf[CoreException])("") {
      config.getAttribute(ZaluumLauncherTab.ATTR_MAIN_BOX, "");
    }
    mainText.setText(mainTypeName);
  }
  override def isValid(config: ILaunchConfiguration): Boolean = {
    setErrorMessage(null);
    setMessage(null);
    val pname = fProjText.getText().trim();
    if (pname.length() > 0) {
      val workspace = ResourcesPlugin.getWorkspace();
      val status = workspace.validateName(pname, IResource.PROJECT);
      if (status.isOK()) {
        val project = ResourcesPlugin.getWorkspace().getRoot().getProject(pname);
        if (!project.exists()) {
          setErrorMessage(MessageFormat.format(LauncherMessages.JavaMainTab_20, Array(pname)));
          return false;
        }
        if (!project.isOpen()) {
          setErrorMessage(MessageFormat.format(LauncherMessages.JavaMainTab_21, Array(pname)));
          return false;
        }
      } else {
        setErrorMessage(MessageFormat.format(LauncherMessages.JavaMainTab_19, Array(status.getMessage)));
        return false;
      }
    }
    val name = mainText.getText().trim();
    if (name.length() == 0) {
      setErrorMessage(LauncherMessages.JavaMainTab_Main_type_not_specified_16);
      return false;
    }
    return true;
  }
  override def performApply(config: ILaunchConfigurationWorkingCopy) {
    config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, fProjText.getText().trim());
    config.setAttribute(ZaluumLauncherTab.ATTR_MAIN_BOX, mainText.getText().trim());
    mapResources(config);
  }
  override def setDefaults(config: ILaunchConfigurationWorkingCopy) {
    val javaElement = getContext();
    if (javaElement != null)
      initializeJavaProject(javaElement, config);
    else
      config.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, "");
    initializeMainBox(javaElement, config);
  }
  def initializeMainBox(origElement: IJavaElement, config: ILaunchConfigurationWorkingCopy) {
    val javaElement = origElement match {
      case member: IMember ⇒ if (member.isBinary) member.getClassFile else member.getCompilationUnit
      case _ ⇒ origElement
    }
    val mainType: Option[IType] = javaElement match {
      case c: ICompilationUnit ⇒ c.getAllTypes.headOption
      case c: IClassFile ⇒ Option(c.getType)
      case _ => None
    }
    mainType foreach { t=>
      val name = t.getFullyQualifiedName
      config.setAttribute(ZaluumLauncherTab.ATTR_MAIN_BOX, name);
      if (!name.isEmpty) {
        val simple = fqNameToSimpleName(name)
        val generated = getLaunchConfigurationDialog.generateName(simple);
        config.rename(generated);
      }
    }
  }
  private def fqNameToSimpleName(fqName: String): String = {
    if (fqName.length() > 0) {
      val index = fqName.lastIndexOf('.')
      if (index > 0) return fqName.substring(index + 1);
    } 
    fqName
  }
}

