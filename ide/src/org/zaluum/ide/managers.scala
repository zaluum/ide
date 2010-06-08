package org.zaluum.ide

import org.eclipse.gef.tools.DirectEditManager
import org.eclipse.gef.GraphicalEditPart
import org.eclipse.jface.fieldassist.ContentProposalAdapter
import org.eclipse.jface.fieldassist.SimpleContentProposalProvider
import org.eclipse.jface.bindings.keys.KeyStroke
import org.eclipse.jface.fieldassist.TextContentAdapter
import org.eclipse.gef.tools.CellEditorLocator

class PortDirectEditManager(source : GraphicalEditPart, locator : CellEditorLocator ) extends DirectEditManager(source, null, locator) {
  def initCellEditor = {
    if(source.isInstanceOf[DirectEditPart]) {
      getCellEditor.setValue(source.asInstanceOf[DirectEditPart].editFigure.getText)
      getCellEditor.getControl.setFont(getEditPart.getFigure.getFont)
      new ContentProposalAdapter(getCellEditor.getControl, new TextContentAdapter, 
        new SimpleContentProposalProvider(Array("P1","P2","P3")), KeyStroke.getInstance("Ctrl+Space"), null)
    }
  }
}

import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.gef.tools.CellEditorLocator
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.swt.widgets.Text
class TextEditorLocator(l:Figure) extends CellEditorLocator {
  val label = l
  override def relocate(celleditor : CellEditor) = {
    val text : Text = celleditor.getControl.asInstanceOf[Text]
    val rect = label.getClientArea
    label.translateToAbsolute(rect);
    val trim = text.computeTrim(0, 0, 0, 0);
    rect.translate(trim.x, trim.y);
    rect.width += trim.width;
    rect.height += trim.height;
    text.setBounds(rect.x, rect.y, rect.width, rect.height);    
  } 
}
