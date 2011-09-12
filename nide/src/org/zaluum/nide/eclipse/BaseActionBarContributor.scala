package org.zaluum.nide.eclipse

import org.eclipse.jface.action.Action
import org.eclipse.jface.action.IMenuManager
import org.eclipse.jface.action.IToolBarManager
import org.eclipse.ui.actions.ActionFactory
import org.eclipse.ui.part.EditorActionBarContributor
import org.eclipse.ui.IActionBars
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.IWorkbenchPage

class BaseActionBarContributor extends EditorActionBarContributor {
  abstract class EditorAction extends Action {
    var editor: GraphicalEditor = _
    def controller = editor.viewer.controller
    def viewer = editor.viewer
    def calcEnabled: Boolean
    val update = () ⇒ setEnabled(calcEnabled)
  }
  abstract class ControllerAction extends EditorAction {
    def setEditor(e: GraphicalEditor) {
      if (editor != null)
        controller.removeListener(update)
      editor = e
      controller.addListener(update)
      update()
    }
  }
  abstract class SelectionAction extends EditorAction {
    def setEditor(e: GraphicalEditor) {
      if (editor != null)
        viewer.selection.removeListener(update)
      editor = e
      viewer.selection.addListener(update)
      update()
    }
  }
  object UndoAction extends ControllerAction {
    override def run() { controller.undo }
    def calcEnabled = controller.canUndo
  }
  object RedoAction extends ControllerAction {
    override def run() { controller.redo }
    def calcEnabled = controller.canRedo
  }
  object DeleteAction extends SelectionAction {
    override def run() { viewer.tool.handleDel() }
    def calcEnabled = !viewer.selection.isEmpty
  }
  object CopyAction extends SelectionAction {
    override def run() { viewer.tool.handleCopy() }
    def calcEnabled = !viewer.selection.isEmpty;
  }
  object CutAction extends SelectionAction {
    override def run() { viewer.tool.handleCut() }
    def calcEnabled = !viewer.selection.isEmpty
  }
  object PasteAction extends SelectionAction {
    override def run() { viewer.tool.handlePaste() }
    def calcEnabled = true
  }
  override def init(bars: IActionBars, page: IWorkbenchPage) {
    super.init(bars, page)
    bars.setGlobalActionHandler(ActionFactory.UNDO.getId, UndoAction);
    bars.setGlobalActionHandler(ActionFactory.REDO.getId, RedoAction);
    bars.setGlobalActionHandler(ActionFactory.DELETE.getId, DeleteAction);
    bars.setGlobalActionHandler(ActionFactory.COPY.getId, CopyAction);
    bars.setGlobalActionHandler(ActionFactory.CUT.getId, CutAction);
    bars.setGlobalActionHandler(ActionFactory.PASTE.getId, PasteAction);
    // remember to add to setActiveEditor below
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
    CopyAction.setEditor(g)
    CutAction.setEditor(g)
    PasteAction.setEditor(g)
  }
}