package org.zaluum.nide.eclipse

import org.eclipse.ui.IEditorPart
import java.io.ByteArrayInputStream
import org.zaluum.nide.model.ProtoModel
import org.eclipse.ui.part.FileEditorInput
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

class GraphicalEditor extends EditorPart {
  
  var viewer : Viewer = _
  def controller = viewer.controller
  def doSave(monitor: IProgressMonitor) {
    val in = new ByteArrayInputStream(ProtoModel.toByteArray(viewer.model))
    inputFile.setContents(in,true,true,monitor);
    controller.markSaved()
    firePropertyChange(IEditorPart.PROP_DIRTY)
  }

  def doSaveAs() {  }

  def init(site: IEditorSite, input: IEditorInput) {
    setSite(site)
    setInput(input)
    setPartName(inputFile.getName)
  }

  def isDirty(): Boolean = { controller.isDirty }

  def isSaveAsAllowed(): Boolean = { false }
  
  def inputFile = getEditorInput.asInstanceOf[FileEditorInput].getFile
  def input = inputFile.getContents(true)
  def createPartControl(parent: Composite){
    val bcp = new BoxClassPath(new File("."),currentThread.getContextClassLoader)
    val model = ProtoModel.read(input)
    input.close()
    val controller = new Controller(model,bcp)
    controller.onExecute {
      firePropertyChange(IEditorPart.PROP_DIRTY)
    }
    viewer = new Viewer(parent,controller) 
  } 

  def setFocus() { viewer.canvas.setFocus }
  
}