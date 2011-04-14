package org.zaluum.nide.eclipse

import org.zaluum.nide.Subject
import org.zaluum.nide.Observer
import org.zaluum.nide.zge.GuiViewer
import org.zaluum.nide.compiler.Parser
import org.zaluum.nide.zge.TreeViewer
import org.zaluum.nide.compiler.BoxDef
import org.zaluum.nide.compiler.Serializer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Location
import org.zaluum.nide.protobuf.BoxFileProtos
import org.eclipse.swt.events.DisposeEvent
import org.eclipse.ui.contexts.IContextService
import org.eclipse.swt.events.ShellEvent
import org.eclipse.swt.events.ShellAdapter
import org.eclipse.swt.events.ShellListener
import org.eclipse.swt.events.DisposeListener
import org.eclipse.ui.handlers.HandlerUtil
import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.commands.ExecutionEvent
import java.io.ByteArrayInputStream
import org.eclipse.core.resources.{IFile, IMarker}
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{Composite, Shell}
import org.eclipse.ui.{IEditorSite, IEditorInput, IEditorPart}
import org.eclipse.ui.ide.IGotoMarker
import org.eclipse.ui.part.{EditorPart, FileEditorInput}
import org.zaluum.nide.zge.{Viewer, Controller}

class GraphicalEditor extends EditorPart with IGotoMarker {

  var viewer: TreeViewer = _
  var shell : Option[Shell] = None 
  def controller = viewer.controller

  def doSave(monitor: IProgressMonitor) {
    val proto = Serializer.proto(viewer.tree.asInstanceOf[BoxDef])
    val in = new ByteArrayInputStream(proto.toByteArray)
    inputFile.setContents(in, true, true, monitor);
    controller.markSaved()
    firePropertyChange(IEditorPart.PROP_DIRTY)
  }

  def doSaveAs() {}

  def init(site: IEditorSite, input: IEditorInput) {
    setSite(site)
    setInput(input)
    setPartName(inputFile.getName)
    val contextService = getSite.getService(classOf[IContextService]).asInstanceOf[IContextService]
    contextService.activateContext("org.zaluum.nide.context")
  }

  def isDirty(): Boolean = { controller.isDirty }

  def isSaveAsAllowed(): Boolean = { false }

  def inputFile = getEditorInput.asInstanceOf[FileEditorInput].getFile
  def project = inputFile.getProject
  def zproject = ZaluumModelMananger.getOrCreate(project)
  def input = inputFile.getContents(true)
  def createPartControl(parent: Composite) {
    val zp = zproject.get // XXX better
    val className = zp.toClassName(inputFile).getOrElse { throw new Exception("Cannot find class name for this file") }
    val proto = BoxFileProtos.BoxClassDef.parseFrom(input)
    val tree = Parser.parse(proto,Some(className)) 
    input.close()
    val controller = new Controller(tree, zp)
    controller.addListener(fireDirty)
    viewer = new TreeViewer(parent, controller, zp)
    controller.registerViewer(viewer)
    // TODO reopen
  }
  val fireDirty: () ⇒ Unit = () ⇒ firePropertyChange(IEditorPart.PROP_DIRTY)
  def setFocus() { viewer.canvas.setFocus }
  def openGUI() {
    if (!shell.isDefined) {
      val newshell = new Shell(getSite.getShell, SWT.MODELESS | SWT.CLOSE | SWT.RESIZE)
      newshell.setLayout(new FillLayout)
      newshell.setText(getTitle + " GUI");
      val guiViewer= new GuiViewer(newshell, controller, zproject.get)
      controller.registerViewer(guiViewer)
      newshell.layout()
      newshell.open()
      
      newshell.addDisposeListener(new DisposeListener(){
        override def widgetDisposed(e:DisposeEvent) {
          guiViewer.dispose()
          controller.unregisterViewer(guiViewer);
          shell = None
        }
      });
      shell = Some(newshell)
    }
  }
  override def dispose() {
    controller.removeListener(fireDirty)
    viewer.dispose()
    controller.dispose()
    shell foreach { s => if (!s.isDisposed) s.dispose } 
  }
  override def gotoMarker(marker: IMarker) {
    val str = marker.getAttribute("BLAME").asInstanceOf[String]
    viewer.gotoMarker(Location.parse(str))
    setFocus
  }
  override def getAdapter(cl: Class[_]) = {
    if (cl == classOf[IGotoMarker]) this
    else super.getAdapter(cl)
  }
}
class OpenGUIHandler extends AbstractHandler {
  override def execute(event : ExecutionEvent) = {
    Option(HandlerUtil.getActiveEditor(event)) match {
      case Some(g:GraphicalEditor) => g.openGUI()
      case _ => 
    }
    null
  }
}