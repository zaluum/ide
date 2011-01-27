package org.zaluum.nide.eclipse

import org.zaluum.nide.model.BoxClassName
import org.zaluum.nide.model.ExtBoxClassName
import org.zaluum.nide.model.Dimension
import java.io.ByteArrayOutputStream
import org.eclipse.core.resources.IFile
import org.eclipse.jface.viewers.{ IStructuredSelection }
import org.eclipse.jface.wizard.Wizard
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.{ Label, Composite }
import org.eclipse.ui.{ INewWizard, IWorkbenchWindow, IWorkbenchPage, IWorkbench }
import org.eclipse.ui.dialogs.WizardNewFileCreationPage
import org.eclipse.ui.ide.IDE
import org.zaluum.nide.model.{ BoxClassDecl, ProtoBuffers }
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
  def className = BoxClassName.parse(this.getFileName) // FIXME
  override protected def getInitialContents = {
    val model = new BoxClassDecl(className, None, true, Dimension(400,300))
    new java.io.ByteArrayInputStream(ProtoBuffers.toByteArray(model))
  }

  def setDescription() {
    this.setTitle("New Box TypedModel Wizard");
    this.setDescription("Creates a new box model file");
    //this.setImageDescriptor(ImageDescriptor.createFromFile(Icons.class,
    //   "icons/banner_64.png")); //$NON-NLS-1$
  }

}
