package org.zaluum.nide.scratch

import java.awt.Graphics
import java.awt.Canvas
import javax.swing.JFrame
import javax.swing.JButton
import java.awt.Frame
import java.awt.FlowLayout
import javax.swing.JPanel
import scala.swing.Swing
object SwingOffscreen {
 def main(args : Array[String]){
    /*val awtWindow = new Frame() {
      
    }*/
    val panel = new JPanel() {
      override def isShowing() = true
      override def isOptimizedDrawingEnabled = false
    }
    val button = new JButton("hola")
    button.setBounds(0,0,50,50)
    panel.add(button)
    panel.setBounds(0,0,100,100);
    panel.doLayout
    val frame = new JFrame()
    //frame.add(panel)
    val canvas = new Canvas() {
      override def paint(g:Graphics){
        panel.paint(g)
        g.drawLine(0,0,100,100);
      }
    }
    frame.add(canvas)
    frame.setVisible(true)
    //awtWindow.show()
 }  
}