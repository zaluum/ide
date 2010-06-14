package org.zaluum.ide

import org.eclipse.gef.ui.parts.ContentOutlinePage;
import org.eclipse.gef.ui.parts.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.gef.EditPartFactory
import org.eclipse.gef.EditPart

class ZaluumOutlinePage(boxEditor : BaseEditor) extends ContentOutlinePage(new TreeViewer()) {
  val editor = boxEditor
  override def createControl(parent : Composite) = {
    super.createControl(parent)
    getViewer.setEditDomain(editor.editDomain)
    getViewer.setEditPartFactory(new EditPartFactory() {
      override def createEditPart(c:EditPart, m:Object) = {
        null
      }
    })
    getViewer.setContents(editor.model)
    //editor.getSelectionSynchronizer.addViewer(getViewer);
    //TODO: Observer
  }

}