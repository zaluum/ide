package org.zaluum.ide
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;
import org.eclipse.ui.ide.IDE;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.zaluum.runtime._
import PersistentModel._
class BoxWizard extends Wizard with INewWizard{

  var page : ZaluumWizardPage = null;
  private var selection : IStructuredSelection = _
  private var workbench : IWorkbench = _
  
  def init(workbench : IWorkbench,currentSelection : IStructuredSelection) {
    this.workbench = workbench;
    selection = currentSelection;
  }
  
  def performFinish = page.finish()
  
  override def addPages() {
    page = new BoxWizardPage(workbench,selection);
    addPage(page);
  }
  
}

object ZaluumWizardPage{
  var fileCount = 1
}
abstract class ZaluumWizardPage(
    pageName:String, 
    selection:IStructuredSelection, 
    val workbench:IWorkbench)
  extends WizardNewFileCreationPage(pageName,selection) {
  setDescription()

  def setDescription()
  
  def finish() : Boolean ={
    val newFile = createNewFile();
    if (newFile == null)
      false
    else{
      val dwindow = workbench.getActiveWorkbenchWindow();
      val page = dwindow.getActivePage();
      if (page != null)
          IDE.openEditor(page, newFile, true);
      ZaluumWizardPage.fileCount+=1
      true
    }
  }
  
}

class BoxWizardPage(
    workbench:IWorkbench, 
    selection:IStructuredSelection) 
    extends ZaluumWizardPage("boxPage",selection,workbench) {

  override def createControl(parent : Composite) {
    super.createControl(parent);
    this.setFileName("emptyModel" + ZaluumWizardPage.fileCount + ".zaluum");
    val composite =getControl().asInstanceOf[Composite];
    new Label(composite, SWT.NONE);
    setPageComplete(validatePage());
  }
  import com.google.common.base.Charsets
  override protected def getInitialContents = {
    val box = new ComposedPBox()
    val model = PModel(box)
    new java.io.ByteArrayInputStream(box.toProto.build.toString.getBytes(Charsets.UTF_8))
  }

  def setDescription() {
    this.setTitle("New Box TypedModel Wizard");
    this.setDescription("Creates a new box model file");
    //this.setImageDescriptor(ImageDescriptor.createFromFile(Icons.class,
     //   "icons/banner_64.png")); //$NON-NLS-1$
  }

}
