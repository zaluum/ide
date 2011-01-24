package org.zaluum.nide.eclipse

import org.eclipse.ui.IEditorPart
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.IPartListener
import org.eclipse.jface.action.Action
import org.eclipse.ui.IWorkbenchPage
import org.eclipse.ui.IActionBars
import org.eclipse.ui.actions.ActionFactory
import org.eclipse.jface.action.{ IMenuManager, IToolBarManager }
import org.eclipse.ui.part.EditorActionBarContributor

class BaseActionBarContributor extends EditorActionBarContributor {
  abstract class EditorAction extends Action  {
    var editor : GraphicalEditor = _
    def setEditor(e:GraphicalEditor) {
     if (editor !=null)
       controller.removeListener(update)
     editor = e
     controller.addListener(update)
     update()
    }
    def controller = editor.viewer.controller
    def calcEnabled : Boolean
    val update =  ()=> setEnabled (calcEnabled)
    
  }

  object UndoAction extends EditorAction{
    override def run() { controller.undo } 
    def calcEnabled=controller.canUndo
  }
  object RedoAction extends EditorAction{
    override def run() { controller.redo } 
    def calcEnabled=controller.canRedo
  }
  object DeleteAction extends EditorAction{
    override def run() { controller.exec(editor.viewer.modelView.createRemoveCommand)}
    def calcEnabled = true//editor.viewer.modelView.createRemoveCommand.canExecute
  }
  override def init(bars: IActionBars, page: IWorkbenchPage) {
    super.init(bars, page)
    bars.setGlobalActionHandler(ActionFactory.UNDO.getId, UndoAction);
    bars.setGlobalActionHandler(ActionFactory.REDO.getId, RedoAction);
    bars.setGlobalActionHandler(ActionFactory.DELETE.getId, DeleteAction);
    // remember to add to setActiveEditor
  }
  
  override def contributeToToolBar(tbm: IToolBarManager) {
    super.contributeToToolBar(tbm)
  }

  override def contributeToMenu(menubar: IMenuManager) {
    super.contributeToMenu(menubar);
  }
  override def setActiveEditor(e: IEditorPart) {
   val g = e.asInstanceOf[GraphicalEditor]
   UndoAction.setEditor(g)
   RedoAction.setEditor(g)
   DeleteAction.setEditor(g)
  }
}