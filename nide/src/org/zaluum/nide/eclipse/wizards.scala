package org.zaluum.nide.eclipse

import org.zaluum.nide.compiler.Serializer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.Dimension
import java.io.ByteArrayOutputStream
import org.eclipse.core.resources.IFile
import org.eclipse.jface.viewers.{ IStructuredSelection }
import org.eclipse.jface.wizard.Wizard
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{ Label, Composite }
import org.eclipse.ui.{ INewWizard, IWorkbenchWindow, IWorkbenchPage, IWorkbench }
import org.eclipse.ui.dialogs.WizardNewFileCreationPage
import org.eclipse.ui.ide.IDE
import org.eclipse.ui.IWorkbenchWindowActionDelegate
import org.eclipse.jface.action.IAction
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jdt.ui.actions.AbstractOpenWizardAction
import org.eclipse.jface.util.IPropertyChangeListener
import org.eclipse.ui.IWorkbenchWindow
import org.eclipse.jdt.internal.ui.wizards.NewElementWizard
import org.eclipse.core.runtime.IExecutableExtension
import org.eclipse.ui.INewWizard
import org.zaluum.nide.Activator
import org.eclipse.jdt.ui.wizards.NewJavaProjectWizardPageOne
import org.eclipse.jdt.ui.wizards.NewJavaProjectWizardPageTwo
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard
import org.eclipse.ui.PlatformUI
import org.eclipse.core.runtime.IConfigurationElement
import org.eclipse.swt.widgets.Shell
import java.lang.reflect.InvocationTargetException
import org.eclipse.jdt.internal.ui.util.ExceptionHandler
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.jface.viewers.StructuredSelection

class BoxWizard extends Wizard with INewWizard {

  var page: ZaluumWizardPage = null;
  private var selection: IStructuredSelection = _
  private var workbench: IWorkbench = _

  def init(workbench: IWorkbench, currentSelection: IStructuredSelection) {
    this.workbench = workbench;
    selection = currentSelection;
  }

  def performFinish = page.finish()

  override def addPages() {
    page = new BoxWizardPage(workbench, selection);
    addPage(page);
  }

}

object ZaluumWizardPage {
  var fileCount = 1
}
abstract class ZaluumWizardPage(
  pageName: String,
  selection: IStructuredSelection,
  val workbench: IWorkbench)
  extends WizardNewFileCreationPage(pageName, selection) {
  setDescription()

  def setDescription()

  def finish(): Boolean = {
    val newFile = createNewFile();
    if (newFile == null)
      false
    else {
      val dwindow = workbench.getActiveWorkbenchWindow();
      val page = dwindow.getActivePage();
      if (page != null)
        IDE.openEditor(page, newFile, true);
      ZaluumWizardPage.fileCount += 1
      true
    }
  }

}

class BoxWizardPage(
  workbench: IWorkbench,
  selection: IStructuredSelection)
  extends ZaluumWizardPage("boxPage", selection, workbench) {

  override def createControl(parent: Composite) {
    super.createControl(parent);
    this.setFileName("emptyModel" + ZaluumWizardPage.fileCount + ".zaluum");
    val composite = getControl().asInstanceOf[Composite];
    new Label(composite, SWT.NONE);
    setPageComplete(validatePage());
  }
  import com.google.common.base.Charsets
  //TODO def className = BoxClassName.parse(this.getFileName) // FIXME
  override protected def getInitialContents = {
    val model = BoxDef(Name(""), None, guiSize = Some(Dimension(250, 250)), None, List(), List(), List(), List(), List())
    new java.io.ByteArrayInputStream(Serializer.proto(model).toByteArray)
  }

  def setDescription() {
    this.setTitle("New Box TypedModel Wizard");
    this.setDescription("Creates a new box model file");
    //this.setImageDescriptor(ImageDescriptor.createFromFile(Icons.class,
    //   "icons/banner_64.png")); //$NON-NLS-1$
  }

}

class ZaluumProjectWizard extends NewElementWizard with IExecutableExtension {
  setDefaultPageImageDescriptor(null)
  setDialogSettings(Activator.getDefault.getDialogSettings)
  setWindowTitle("New Zaluum Project")
  var firstPage: NewJavaProjectWizardPageOne = _
  var secondPage: NewJavaProjectWizardPageTwo = _
  var configElement: IConfigurationElement = _

  override def addPages {
    super.addPages()
    firstPage = new NewJavaProjectWizardPageOne
    addPage(firstPage)
    firstPage.setTitle("Create Zaluum Project")
    firstPage.setDescription("Create a new Zaluum project")
    secondPage = new NewJavaProjectWizardPageTwo(firstPage)
    secondPage.setTitle("Build settings")
    secondPage.setDescription("Build settings")
    addPage(secondPage)
  }
  override def finishPage(monitor: IProgressMonitor) {
    secondPage.performFinish(monitor)
  }
  override def performFinish  = {
    var res = super.performFinish
    if (res) {
      BasicNewProjectResourceWizard.updatePerspective(configElement);
      val project = secondPage.getJavaProject().getProject();
      selectAndReveal(project);

      val workingSets = firstPage.getWorkingSets();
      if (workingSets.length > 0) {
        PlatformUI.getWorkbench().getWorkingSetManager().addToWorkingSets(project, workingSets);
      }

      res = finalizeNewProject(project);
    }
    res
  }
  override def handleFinishException(shell: Shell, e: InvocationTargetException) {
    ExceptionHandler.handle(e, getShell, "New Zaluum Project Error", "New Zaluum Project Error")
  }
  private def finalizeNewProject(project: IProject) = {
    // add zaluum runtime
    try {
      ZaluumNature.addNature(project)
      project.build(IncrementalProjectBuilder.FULL_BUILD, null)
    } catch { case e ⇒ }
    selectAndReveal(project)
    true
  }
  override def setInitializationData(cfig: IConfigurationElement, propertyName: String, data: AnyRef) {
    configElement = cfig
  }
  override def performCancel = {
    secondPage.performCancel
    super.performCancel
  }
  override def getCreatedElement = {
    secondPage.getJavaProject
  }
}

class OpenZaluumProjectWizardAction extends AbstractOpenWizardAction with IWorkbenchWindowActionDelegate {

  def dispose(): Unit = {}

  def init(window: IWorkbenchWindow) {
    setShell(window.getShell)
  }

  def run(action: IAction): Unit = { super.run() }

  def selectionChanged(action: IAction, selection: ISelection): Unit = {
    selection match {
      case i: IStructuredSelection ⇒ setSelection(i)
      case _ ⇒ setSelection(StructuredSelection.EMPTY)
    }
  }

  protected def createWizard(): INewWizard = { new ZaluumProjectWizard() }

  override def doCreateProjectFirstOnEmptyWorkspace(shell: Shell) = true

}
