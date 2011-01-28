package org.zaluum.nide.model

import javax.swing.JComponent

case class BoxClass(
    val className: BoxClassName, 
    val scala: Boolean = false, 
    val image: String, 
    var guiCreator: Option[() ⇒ JComponent] = None, 
    var visual: Boolean) {
  var innerClasses = Set[BoxClass]()
  var ports = Set[TypedPort]()
  def port(s: String) = ports find { _.name == s }
  def isInner = className.isInstanceOf[InnerBoxClassName]
  override def toString = "boxClass[" + className + "]"
}
case class TypedPort(val descriptor: String, val in: Boolean, val name: String, val pos: Point)

