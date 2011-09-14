package org.zaluum.nide.eclipse.launch

import scala.collection.mutable.Buffer

import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IAdaptable
import org.eclipse.debug.core.DebugPlugin
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.debug.ui.DebugUITools
import org.eclipse.debug.ui.ILaunchShortcut
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.debug.ui.launcher.DebugTypeSelectionDialog
import org.eclipse.jdt.internal.debug.ui.JDIDebugUIPlugin
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.window.Window
import org.eclipse.ui.IEditorPart
import org.zaluum.annotation.Box
import org.zaluum.nide.utils.JDTUtils._
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnit
import org.zaluum.nide.eclipse.GraphicalEditor

class ZaluumLaunchShortcut extends ILaunchShortcut {

  def launch(selection: ISelection, mode: String): Unit = {
    selection match {
      case s: IStructuredSelection ⇒
        if (!s.isEmpty)
          s.getFirstElement match {
            case jp: JavaProject ⇒
              val tpe = choose(search(patternAnnotation(classOf[Box].getName), sourcesScope(jp), null))
              tpe foreach { t ⇒
                doLaunch(t.getCompilationUnit.asInstanceOf[ZaluumCompilationUnit], mode)
              }
            case a: IAdaptable ⇒
              val cu = a.getAdapter(classOf[ZaluumCompilationUnit]).asInstanceOf[ZaluumCompilationUnit]
              if (cu != null)
                doLaunch(cu, mode)
            case _ ⇒
          }
    }
  }
  def choose(types: List[IType]): Option[IType] = {
    val mmsd = new DebugTypeSelectionDialog(JDIDebugUIPlugin.getShell(), types.toArray, "Choose Main Zaluum Class")
    if (mmsd.open() == Window.OK) {
      Option(mmsd.getResult()(0).asInstanceOf[IType])
    } else
      None
  }
  def launch(editor: IEditorPart, mode: String): Unit = {
    editor match {
      case g: GraphicalEditor ⇒
        val cu = g.controller.cu.asInstanceOf[ZaluumCompilationUnit]
        doLaunch(cu, mode)
      case _ ⇒
    }
  }
  def launchConfigurationType =
    DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurationType("org.zaluum.nide.launchZaluumConfigurationType");

  def doLaunch(cu: ZaluumCompilationUnit, mode: String) {
    val config = findLaunchConfiguration(cu).getOrElse(createLaunchConfiguration(cu))
    DebugUITools.launch(config, mode);
  }
  private def launchManager = DebugPlugin.getDefault().getLaunchManager();
  protected def createLaunchConfiguration(cu: ZaluumCompilationUnit): ILaunchConfiguration = {
    val tpe = cu.getAllTypes()(0)
    val wc = launchConfigurationType.newInstance(null, launchManager.generateLaunchConfigurationName(tpe.getTypeQualifiedName('.')));
    wc.setAttribute(ZaluumLauncherTab.ATTR_MAIN_BOX, tpe.getFullyQualifiedName());
    wc.setAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, tpe.getJavaProject().getElementName());
    wc.setMappedResources(Array(tpe.getUnderlyingResource()))
    wc.doSave();
  }
  protected def findLaunchConfiguration(cu: ZaluumCompilationUnit) = {
    val candidateConfigs = Buffer[ILaunchConfiguration]()
    val configs = DebugPlugin.getDefault().getLaunchManager().getLaunchConfigurations(launchConfigurationType)
    val name = cu.getAllTypes()(0).getFullyQualifiedName
    for (config ← configs) {
      if (config.getAttribute(ZaluumLauncherTab.ATTR_MAIN_BOX, "").equals(name)) {
        if (config.getAttribute(IJavaLaunchConfigurationConstants.ATTR_PROJECT_NAME, "").equals(cu.getJavaProject().getElementName())) {
          candidateConfigs += config
        }
      }
    }
    candidateConfigs.headOption // TODO choose launch mode
  }

  def getLaunchableResource(selection: ISelection): IResource = {
    selection match {
      case s: IStructuredSelection ⇒
        if (s.size != 1) null
        else {
          s.getFirstElement match {
            case z: ZaluumCompilationUnit ⇒
              z.getResource
            case a ⇒ null
          }
        }
    }
  }

  def getLaunchableResource(editorPart: IEditorPart): IResource = {
    editorPart match {
      case g: GraphicalEditor ⇒ g.inputFile
      case _                  ⇒ null
    }
  }
}