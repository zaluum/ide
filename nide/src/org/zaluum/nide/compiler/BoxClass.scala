package org.zaluum.nide.compiler

import org.zaluum.nide.model.BoxClassDecl
import org.zaluum.nide.model.Point
import org.zaluum.nide.model.Dimension
import org.zaluum.nide.model.InnerBoxClassName
import org.zaluum.nide.model.BoxClassName
import javax.swing.JComponent

object PresentationCompiler {

  def toBoxClass(bcd: BoxClassDecl): BoxClass = {
    val bc = BoxClass(
      bcd.className,
      scala = false,
      image = bcd.imageName.getOrElse(""),
      bcd.visual ); 
    bc.ports = bcd.portDecls map { pd ⇒ TypedPort(pd.descriptor, pd.in, pd.name, pd.pos) }
    bc.innerClasses = bcd.innerClassDecls map { toBoxClass(_)}
    bc
  }
  
  def createGUI(bcd: BoxClassDecl,bcp:BoxClassPath) : JComponent= {
    import javax.swing.JPanel
    val component = new JPanel(null)
    component.setSize(bcd.guiSize.w, bcd.guiSize.h)
    for (i ← bcd.boxes) {
      if (i.boxClassName != bcd.className) { // check cycles!
        val c = bcp.findGuiCreator(i.boxClassName) map { _() }
        c foreach { child ⇒
          val pos = i.guiPos.map {_.pos} getOrElse (Point(0,0))
          val size = i.guiPos.map {_.size} getOrElse (Dimension(50,50))
          child.setBounds(pos.x, pos.y, size.w, size.h);
          component.add(child)
        }
      }
    }
    component
  }

}
case class BoxClass(
  val className: BoxClassName,
  val scala: Boolean = false,
  val image: String,
  var visual: Boolean) {
  var innerClasses = Set[BoxClass]()
  var ports = Set[TypedPort]()
  def port(s: String) = ports find { _.name == s }
  def isInner = className.isInstanceOf[InnerBoxClassName]
  override def toString = "boxClass[" + className + "]"
}
case class TypedPort(val descriptor: String, val in: Boolean, val name: String, val pos: Point)

