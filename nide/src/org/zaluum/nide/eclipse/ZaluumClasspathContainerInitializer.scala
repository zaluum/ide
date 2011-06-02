package org.zaluum.nide.eclipse

import org.eclipse.jdt.core.ClasspathContainerInitializer
import org.eclipse.core.runtime.IPath
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.IClasspathContainer
import org.zaluum.nide.Activator
import org.eclipse.jdt.ui.wizards.IClasspathContainerPage
import org.eclipse.jdt.ui.wizards.NewElementWizardPage
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.internal.ui.JavaPluginImages
import org.eclipse.jdt.core.IClasspathEntry
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.SWT

class ZaluumClasspathContainerInitializer extends ClasspathContainerInitializer {
  def entries = {
    val plugin = Activator.plugin
    import plugin._
    Array(JavaCore.newLibraryEntry(libZaluumRuntime.get, libZaluumRuntimeSrc.getOrElse(null), null))
  }
  /*def entries = {
    val plugin = Activator.plugin
    import plugin._
    plugin.runtimeJars map { url =>
      val sourceAttach = plugin.locateSourceAttachment(url) map { surl => new Path(surl.getPath)} getOrElse (null) 
      JavaCore.newLibraryEntry(new Path(url.getPath), sourceAttach, null)
    } toArray
  }*/
  def initialize(containerPath: IPath, project: IJavaProject): Unit = {
    val fix = entries
     JavaCore.setClasspathContainer(containerPath, 
            Array(project), 
            Array(new IClasspathContainer {
              def getPath = containerPath
              def getClasspathEntries = fix
              def getDescription = "Zaluum Library Container ["+Activator.plugin.libZaluumRuntimeBundle.getVersion.toString+"]"
              def getKind = IClasspathContainer.K_APPLICATION
            }), null);
  }

}


abstract class ZaluumClasspathContainerPage(id : String, name : String, title : String, desc : String) extends NewElementWizardPage(name) with IClasspathContainerPage {
  val fContainerEntryResult = JavaCore.newContainerEntry(new Path(id))

  setTitle(title)
  setDescription(desc)
  setImageDescriptor(JavaPluginImages.DESC_WIZBAN_ADD_LIBRARY)

  def finish() = true

  def getSelection() : IClasspathEntry = fContainerEntryResult

  def setSelection(containerEntry : IClasspathEntry) {}

  def createControl(parent : Composite) {
    val composite = new Composite(parent, SWT.NONE)
    setControl(composite)
  }
}

class ZaluumLibraryClasspathContainerPage extends
  ZaluumClasspathContainerPage(Activator.plugin.zaluumLibId,
        "ZaluumLibraryContainerPage",
    "Zaluum Library Container",
    "Zaluum library container") 