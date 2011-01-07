package org.zaluum.nide.zge

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

  def createPartControl(parent: Composite){ viewer = new Viewer(parent,null) } // FIXME null

  def setFocus() {  }
  
}