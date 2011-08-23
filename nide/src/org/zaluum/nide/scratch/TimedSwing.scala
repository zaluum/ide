package org.zaluum.nide.scratch
import javax.swing.JPanel
import java.awt.Component
import javax.swing.JSlider
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.SwingWorker
import javax.swing.SwingUtilities
import javax.swing.event.ChangeListener
import javax.swing.event.ChangeEvent
import javax.swing.JTextField
import javax.swing.JLabel
import java.util.concurrent.atomic.AtomicReference
import scala.collection.mutable.Buffer
import javax.swing.Timer
import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import java.util.concurrent.CopyOnWriteArrayList

object Runner {
  private var b = Set[TimedSwing]()
  def mark(t:TimedSwing) {
    this.synchronized {
      b += t
    }
  }
  def run {
    new Timer(10, new ActionListener {
       def actionPerformed(e:ActionEvent) {
         val oldb = Runner.this.synchronized {
           if (b.isEmpty) return;
           val oldb = b
           b = Set[TimedSwing]()
           oldb
         }
         for (t <- oldb) t.updateGUI()
       }
    })
  }
}
trait TimedSwing {
  def updateGUI()  
  def widget : Component
  def markDirty() {
    Runner.mark(this)
  }
}
class WidgetWrite extends TimedSwing{
  val widget = new JLabel
  val ref = new AtomicReference[String]
  def run(s:String) = {
    if (ref.getAndSet(s)!=s)
      markDirty()
  }
  def updateGUI() {
    widget.setText(ref.get)
  }
}
class WidgetRead extends TimedSwing{
  val widget = new JSlider
  widget.addChangeListener(new ChangeListener {
    def stateChanged(e: ChangeEvent) {
    i.set(widget.getValue())
    }
  })
  val i = new AtomicInteger
  def run = {
    i.get
  }
  def updateGUI {}
}

