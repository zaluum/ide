package org.zaluum.nide.scratch

import org.zaluum.nide.zge.SWTUtils
import javax.swing.JComponent
import java.awt.BorderLayout
import javax.swing.JPanel
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.Figure
import java.awt.{Graphics => AG}
import java.awt.image.BufferedImage
import org.eclipse.swt.widgets.Display
class SwingFigure(val component:JComponent) extends Figure {
  setOpaque(true)
  override def paintFigure(g:Graphics) {
    val rect = getClientArea()
    component.setBounds(0,0,rect.width,rect.height);
    component.doLayout
    val aimage = new BufferedImage(rect.width,rect.height,BufferedImage.TYPE_INT_RGB)
    val ag = aimage.createGraphics
    component.paint(ag)
    val imageData = SWTUtils.convertAWTImageToSWT(aimage)
    val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(),imageData)
    g.drawImage(image,rect.x,rect.y)
    ag.dispose();    
    image.dispose()
  }
}