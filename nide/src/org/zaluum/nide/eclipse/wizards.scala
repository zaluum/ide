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
import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.IStatus
import org.eclipse.core.runtime.SubProgressMonitor
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IMethod
import org.eclipse.jdt.core.IPackageDeclaration
import org.eclipse.jdt.core.IPackageFragment
import org.eclipse.jdt.core.IPackageFragmentRoot
import org.eclipse.jdt.core.ISourceRange
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.JavaModelException
import org.eclipse.jdt.internal.core.ClasspathEntry
import org.eclipse.jdt.internal.core.util.Util
import org.eclipse.jdt.internal.ui.dialogs.StatusInfo
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages
import org.eclipse.jdt.internal.ui.wizards.dialogfields.SelectionButtonDialogFieldGroup
import org.eclipse.jdt.ui.wizards.NewTypeWizardPage
import org.eclipse.swt.widgets.Composite
import org.eclipse.text.edits.DeleteEdit
import org.eclipse.text.edits.ReplaceEdit
import org.eclipse.text.edits.TextEdit
import org.eclipse.jdt.ui.wizards.NewClassWizardPage
import org.zaluum.nide.eclipse.integration.ReflectionUtils
import java.nio.charset.Charset
import org.eclipse.jdt.internal.ui.JavaPluginImages
import org.eclipse.jdt.internal.ui.JavaPlugin
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.jdt.core.JavaCore
import org.eclipse.core.runtime.Path
import scala.collection.mutable.ArrayBuffer

class ZaluumProjectWizard extends NewElementWizard with IExecutableExtension {
  setDefaultPageImageDescriptor(null)
  setDialogSettings(Activator.getDefault.getDialogSettings)
  setWindowTitle("New Zaluum Project")
  var firstPage: NewJavaProjectWizardPageOne = _
  var secondPage: NewJavaProjectWizardPageTwo = _
  var configElement: IConfigurationElement = _

  override def addPages {
    super.addPages()
    firstPage = new NewJavaProjectWizardPageOne() {
    override def getDefaultClasspathEntries() : Array[IClasspathEntry] = {
        val others = ArrayBuffer(super.getDefaultClasspathEntries : _*)
        others += JavaCore.newContainerEntry(Path.fromPortableString(Activator.plugin.zaluumLibId))
        others.toArray
      }
    }
    addPage(firstPage)
    firstPage.setTitle("Create Zaluum Project")
    firstPage.setDescription("Create a new Zaluum Project")
    secondPage = new NewJavaProjectWizardPageTwo(firstPage) 
    secondPage.setTitle("Build settings")
    secondPage.setDescription("Build settings")
    addPage(secondPage)
  }
  override def finishPage(monitor: IProgressMonitor) {
    secondPage.performFinish(monitor)
  }
  override def performFinish = {
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
    ExceptionHandler.handle(e, getShell, "Error creating Zaluum Project", "An error occoured while creating the Zaluum Project")
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
abstract class OpenWizardAction extends AbstractOpenWizardAction with IWorkbenchWindowActionDelegate {

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
}
class OpenZaluumProjectWizardAction extends OpenWizardAction {
  protected def createWizard(): INewWizard = { new ZaluumProjectWizard() }
  override def doCreateProjectFirstOnEmptyWorkspace(shell: Shell) = true
}
class OpenZaluumNewClassWizardAction extends OpenWizardAction {
  protected def createWizard(): INewWizard = { new ZaluumNewClassWizard() }  
}
class ZaluumNewClassWizard extends NewElementWizard {
  var fPage: ZNewClassWizardPage = _

  setDefaultPageImageDescriptor(JavaPluginImages.DESC_WIZBAN_NEWCLASS);
  setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
  setWindowTitle("Create a new Zaluum class");

  override def addPages() {
    super.addPages();
    fPage = new ZNewClassWizardPage();
    addPage(fPage);
    fPage.init(getSelection());
  }

  override protected def canRunForked() = {
    !fPage.isEnclosingTypeSelected();
  }

  protected def finishPage(monitor: IProgressMonitor) {
    fPage.createType(monitor); // use the full progress monitor
  }

  override def performFinish() = {
    warnAboutTypeCommentDeprecation();
    var res = super.performFinish();
    if (res) {
      val resource = fPage.getModifiedResource();
      if (resource != null) {
        selectAndReveal(resource);
        openResource(resource.asInstanceOf[IFile]);
      }
    }
    res;
  }
  def getCreatedElement() = fPage.getCreatedType();
}
class ZNewClassWizardPage extends NewClassWizardPage {
  setTitle("Zaluum Class")
  setDescription("Create a new Zaluum class")

  //private static final int FINAL_INDEX = 1;

  private var fStatus: IStatus = _

  override protected def getCompilationUnitName(typeName: String) = typeName + ".zaluum"

  override def createControl(parent: Composite) {
    initializeDialogUnits(parent);

    val composite = new Composite(parent, SWT.NONE);
    composite.setFont(parent.getFont());

    val nColumns = 4;

    val layout = new org.eclipse.swt.layout.GridLayout();
    layout.numColumns = nColumns;
    composite.setLayout(layout);

    createContainerControls(composite, nColumns);
    createPackageControls(composite, nColumns);

    createSeparator(composite, nColumns);

    createTypeNameControls(composite, nColumns);

    //createSuperClassControls(composite, nColumns);

    setControl(composite);

    org.eclipse.jface.dialogs.Dialog.applyDialogFont(composite);
    PlatformUI.getWorkbench().getHelpSystem().setHelp(composite, org.eclipse.jdt.internal.ui.IJavaHelpContextIds.NEW_CLASS_WIZARD_PAGE);
  }

  override protected def createTypeMembers(tpe: IType,
    imports: NewTypeWizardPage.ImportsManager, monitor: IProgressMonitor) {
    super.createTypeMembers(tpe, imports, monitor);
    if (isCreateMain()) {
      // replace main method with a more groovy version
      val main = tpe.getMethod("main", Array[String]("[QString;"));
      if (main != null && main.exists()) {
        main.delete(true, monitor);
        tpe.createMethod("static main(args) {\n\n}", null, true, monitor);
      }
    }
  }

  override protected def typeNameChanged(): IStatus = {
    val status = super.typeNameChanged().asInstanceOf[StatusInfo];
    val pack = getPackageFragment();
    if (pack == null) {
      return status;
    }

    val project = pack.getJavaProject();
    try {
      if (!project.getProject().hasNature(ZaluumNature.NATURE_ID)) {
        status
          .setWarning(project.getElementName()
            + " is not a zaluum project.  Zaluum Nature will be added to project upon completion.");
      }
    } catch {
      case e: CoreException ⇒
        status.setError("Exception when accessing project natures for "
          + project.getElementName());
    }

    val typeName = getTypeNameWithoutParameters();
    // must not exist as a .zaluum file
    if (status.getSeverity() < IStatus.ERROR) {
      if (pack != null) {
        var tpe: IType = null;
        try {
          tpe = project.findType(pack.getElementName(), typeName);
        } catch {
          case e: JavaModelException ⇒
          // can ignore
        }
        if (tpe != null && tpe.getPackageFragment().equals(pack)) {
          status
            .setError(NewWizardMessages.NewTypeWizardPage_error_TypeNameExists);
        }
      }
      // check exclusion filters
      try {
        val entry = pack
          .getParent().asInstanceOf[IPackageFragmentRoot].getRawClasspathEntry().asInstanceOf[ClasspathEntry];
        if (entry != null) {
          val inclusionPatterns = entry.fullInclusionPatternChars();
          val exclusionPatterns = entry.fullExclusionPatternChars();
          if (Util.isExcluded(
            pack.getResource().getFullPath()
              .append(getCompilationUnitName(typeName)), inclusionPatterns,
            exclusionPatterns, false)) {
            status
              .setError("Cannot create Zaluum type because of exclusion patterns on the source folder.");
          }

        }
      } catch {
        case e: JavaModelException ⇒
          status.setError(e.getLocalizedMessage());
          Activator.logError("Exception inside new Zaluum class wizard", e);
      }
    }

    return status;
  }

  override def createType(monitor: IProgressMonitor) {
    var pack = getPackageFragment();
    if (pack != null) {
      val project = pack.getJavaProject().getProject();
      if (!ZaluumNature.hasZaluumNature(project)) {
        // add groovy nature
        ZaluumNature.addNature(project);
      }
    }

    val root = getPackageFragmentRoot();
    if (pack == null) {
      pack = root.getPackageFragment(""); //$NON-NLS-1$
    }

    if (!pack.exists()) {
      val packName = pack.getElementName();
      pack = root.createPackageFragment(packName, true, new SubProgressMonitor(monitor, 1));
    } else {
      monitor.worked(1);
    }
    def getInitialContents = {
      val model = BoxDef(Name(""), Name(pack.getElementName),None, guiSize = Some(Dimension(250, 250)), None, List(), List(), List(), List(), List())
      val bytes = Serializer.proto(model).toByteArray
      new String(bytes, Charset.forName("ISO-8859-1"))
    }
    val cuName = getCompilationUnitName(getTypeNameWithoutParameters())
    val contents = getInitialContents
    val cu = pack.createCompilationUnit(cuName, "", true, monitor)

    val res = cu.getResource.asInstanceOf[IFile]
    res.setCharset("ISO-8859-1", null)
    cu.becomeWorkingCopy(null)
    cu.applyTextEdit(new ReplaceEdit(0, cu.getBuffer.getLength, contents), null)
    cu.commitWorkingCopy(true, new SubProgressMonitor(monitor, 1));
    cu.discardWorkingCopy
    monitor.done()
  }

  def getOtherModifierButtonsFieldGroup(): SelectionButtonDialogFieldGroup = {
    ReflectionUtils.getPrivateField(
      classOf[NewTypeWizardPage], "fOtherMdfButtons", this).asInstanceOf[SelectionButtonDialogFieldGroup];
  }

  private def getTypeNameWithoutParameters(): String = {
    val typeNameWithParameters = getTypeName();
    val angleBracketOffset = typeNameWithParameters.indexOf('<');
    if (angleBracketOffset == -1) {
      return typeNameWithParameters;
    } else {
      return typeNameWithParameters.substring(0, angleBracketOffset);
    }
  }

  override def getModifiers() = {
    var modifiers = super.getModifiers();
    modifiers &= ~F_PUBLIC;
    modifiers &= ~F_PRIVATE;
    modifiers &= ~F_PROTECTED;
    modifiers &= ~F_FINAL;
    modifiers;
  }

  def getStatus(): IStatus = fStatus

  override protected def updateStatus(status: IStatus) {
    super.updateStatus(status);
    fStatus = status;
  }

}

