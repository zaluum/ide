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
  private var toUpdateMap = Map[AnyRef, () ⇒ Unit]()
  def run {
    new Timer(10, new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val oldb = Runner.this.synchronized {
          if (toUpdateMap.isEmpty) return ;
          val oldb = toUpdateMap
          toUpdateMap = toUpdateMap.empty
          oldb
        }
        for (runner ← oldb.values) runner()
      }
    })
  }
  def fastUpdate(source: AnyRef)(r: ⇒ Unit) {
    val rr = () ⇒ r
    toUpdateMap += (source -> rr)
  }
}

class WidgetWrite {
  val widget = new JLabel
  val ref = new AtomicReference[String]
  def run(s: String) = {
    if (ref.getAndSet(s) != s)
      Runner.fastUpdate(this) {
        widget.setText(ref.get)
      }

  }
}
class WidgetRead {
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
}

