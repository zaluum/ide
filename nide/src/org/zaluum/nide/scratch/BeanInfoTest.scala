package org.zaluum.nide.scratch
import java.beans.Introspector
import java.beans.BeanInfo

object BeanInfoTest {
  def main(args: Array[String]) {
    val b: BeanInfo = Introspector.getBeanInfo(classOf[javax.swing.JButton])
    println(b.getBeanDescriptor())
    b.getPropertyDescriptors.foreach { println }
  }
}