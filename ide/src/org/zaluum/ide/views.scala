package org.zaluum.ide

import org.eclipse.gef.ui.views.palette.PaletteViewerPage
import org.eclipse.gef.ui.views.palette.PaletteView
import org.eclipse.gef.ui.parts.ScrollingGraphicalViewer
import org.eclipse.gef.ui.palette.PaletteViewerProvider
import org.eclipse.jface.action.IToolBarManager
import org.eclipse.jface.action.Action
import org.eclipse.jface.action.IAction
import org.eclipse.ui.IWorkbenchPage
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.part.IPage
import org.eclipse.ui.part.MessagePage
import org.eclipse.ui.part.PageBookView
import org.eclipse.ui.part.PageBook
import org.eclipse.swt.widgets.Composite
import org.eclipse.draw2d.ColorConstants

/**
 * @author matias
 */
object BoxFaceView {
    val ID = "zaluum.boxFaceView"
}
class BoxFaceView extends PaletteView {
  override def isImportant(part : IWorkbenchPart ) = { part.isInstanceOf[Editor] }
  override def createDefaultPage(book : PageBook) = {
    val messagePage = new MessagePage
    messagePage.createControl(getPageBook)
    initPage(messagePage)
    messagePage
  }
  override def doCreatePage(part : IWorkbenchPart) = {
    val editor = part.asInstanceOf[Editor]
    val page = new BoxFacePage(editor)
    initPage(page)
    page.createControl(getPageBook)
    //new PageRec(part, page)
    null
  }
  override def getBootstrapPart() : IWorkbenchPart = {
    val page = getSite.getPage
    if(page!= null)
        return page.getActiveEditor
    null
  }
  class BoxFacePage(e : Editor) extends PaletteViewerPage(new PaletteViewerProvider(e.editDomain)) {
    val editor = e 
    val graphicalViewer = new ScrollingGraphicalViewer()     
    
    override def setFocus() = { graphicalViewer.getControl.setFocus }
    override def getControl() = { graphicalViewer.getControl }
    override def createControl(parent : Composite) = {
      val rootBox = editor.modelEditPart
      graphicalViewer.createControl(parent)
      graphicalViewer.setEditDomain(editor.editDomain)
      graphicalViewer.getControl().setBackground(ColorConstants.listBackground);
      graphicalViewer.getControl().setForeground(ColorConstants.lightGray);
      //editor.getModelEditPart().addObserver(this);
      //graphicalViewer.setEditPartFactory(new BoxFaceEditPartFactory());
      //ScalableFreeformRootEditPart rootEditPart = new ScalableFreeformRootEditPart();
      //graphicalViewer.setRootEditPart(rootEditPart);
      //graphicalViewer.setContents(new FaceModel(rootBox));
      getSite().setSelectionProvider(graphicalViewer);
      val mgr = getSite.getActionBars.getToolBarManager
      mgr.add(new Action() { 
        override def run() { }
      }.setImageDescriptor(Activator.getDefault().getImageRegistry().getDescriptor("portin_16")).asInstanceOf[IAction])
      mgr.add(new Action() { 
        override def run() { }
      }.setImageDescriptor(Activator.getDefault().getImageRegistry().getDescriptor("portin_32")).asInstanceOf[IAction])
      getSite().getActionBars().updateActionBars();    
    }
  }
}
