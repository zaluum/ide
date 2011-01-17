package org.zaluum.nide.eclipse

import org.eclipse.ui.part.EditorActionBarContributor
import org.eclipse.jface.action.IToolBarManager
import org.eclipse.jface.action.IMenuManager

class BaseActionBarContributor extends EditorActionBarContributor {

  override def contributeToToolBar(tbm: IToolBarManager) {
    super.contributeToToolBar(tbm)
  }

  override def contributeToMenu(menubar: IMenuManager) {
    super.contributeToMenu(menubar);
  }

}