package org.zaluum.nide.eclipse
import org.eclipse.jface.action.{ IMenuManager, IToolBarManager }
import org.eclipse.ui.part.EditorActionBarContributor

class BaseActionBarContributor extends EditorActionBarContributor {

  override def contributeToToolBar(tbm: IToolBarManager) {
    super.contributeToToolBar(tbm)
  }

  override def contributeToMenu(menubar: IMenuManager) {
    super.contributeToMenu(menubar);
  }

}