package org.zaluum.nide.eclipse

import org.zaluum.nide.zge.Controller
import org.zaluum.nide.zge.Viewer
import org.zaluum.nide.model.Example
import java.io.File
import org.zaluum.nide.compiler.BoxClassPath
import org.eclipse.ui.IEditorInput
import org.eclipse.ui.IWorkbenchPartSite
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.IEditorSite
import org.eclipse.ui.IPropertyListener
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.part.EditorPart
import org.eclipse.ui.ISelectionListener
import org.eclipse.swt.graphics.Image
import org.eclipse.jface.viewers.ISelection

class GraphicalEditor extends EditorPart with ISelectionListener {
  
  var viewer : Viewer = _
  def selectionChanged(part: IWorkbenchPart, selection: ISelection) {  }

  def doSave(monitor: IProgressMonitor) {  }

  def doSaveAs() {  }

  def init(site: IEditorSite, input: IEditorInput) {
    setSite(site)
    setInput(input)
  }

  def isDirty(): Boolean = { false }

  def isSaveAsAllowed(): Boolean = { false }

  def createPartControl(parent: Composite){
    val bcp = new BoxClassPath(new File("."),currentThread.getContextClassLoader)
    val model = Example.sumsumModel
    val controller = new Controller(model,bcp)
    viewer = new Viewer(parent,controller) 
  } 

  def setFocus() {  }
  
}