package org.zaluum.nide.compiler

import org.zaluum.nide.model.Point
import org.zaluum.nide.model.Dimension
import javax.swing.JComponent

  
/*  def createGUI(bcd: BoxClassDecl,bcp:BoxClassPath) : JComponent= {
    import javax.swing.JPanel
    val component = new JPanel(null)
    component.setSize(bcd.guiSize.w, bcd.guiSize.h)
    for (i ← bcd.boxes) {
      if (i.boxClassName != bcd.className) { // check cycles!
        /*TODO val c = bcp.findGuiCreator(i.boxClassName) map { _() }
        c foreach { child ⇒
          val pos = i.guiPos.map {_.pos} getOrElse (Point(0,0))
          val size = i.guiPos.map {_.size} getOrElse (Dimension(50,50))
          child.setBounds(pos.x, pos.y, size.w, size.h);
          component.add(child)
        }*/
      }
    }
    component
  }

}*/
