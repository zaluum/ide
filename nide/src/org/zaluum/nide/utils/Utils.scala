package org.zaluum.nide.utils
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.swt.widgets.{ Text, Display }
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.draw2d.Figure
import org.eclipse.jface.fieldassist.IContentProposalProvider
import org.eclipse.jface.fieldassist.IContentProposal
import scala.collection.JavaConversions._
import scala.collection.mutable._
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.IStatus
object NotImplemented {
  def ??? = sys.error("not implemented")
}
object Utils {
  def loadIcons(ir: ImageRegistry, base: Class[_], keys: String*) = {
    keys.foreach(k ⇒ { loadImage(ir, k + "_16", base); loadImage(ir, k + "_32", base) })
  }

  def loadImage(ir: ImageRegistry, key: String, base: Class[_]) = {
    import org.eclipse.jface.resource.ImageDescriptor
    if (ir.getDescriptor(key) == null) {
      ir.put(key, ImageDescriptor.createFromURL(base.getResource(key + ".png")));
    }
  }
  implicit def asRunnable(func: ⇒ Unit): Runnable = {
    new Runnable() {
      def run() {
        func
      }
    }
  }
  def inSWT(toRun: ⇒ Unit)(implicit display: Display) {
    if (Display.getCurrent != null)
      toRun
    else {
      (if (display == null)
        Display.getDefault
      else display).asyncExec(new Runnable { override def run { toRun } })
    }
  }
  def job(name: String)(a: IProgressMonitor ⇒ IStatus) =
    new Job(name) {
      protected def run(monitor: IProgressMonitor): IStatus = a(monitor)
    }
}

class EditCPP(val c: Array[String]) extends IContentProposalProvider() {
  override def getProposals(contentsProposal: String, position: Int) =
    Array.tabulate(c.size)(i ⇒ new ContentProposal(i, c(i)))
}

class ContentProposal(index: Int, text: String) extends IContentProposal {
  override def getContent(): String = { text }
  override def getCursorPosition(): Int = { index }
  override def getLabel(): String = { text }
  override def getDescription(): String = {
    if (text.startsWith("@")) "Connect to outer port " + text.substring(1) else "Connect to neighbor port " + text
  }
}

class RichCast(val a: AnyRef) {
  def castOption[A](implicit m: Manifest[A]) =
    if (Manifest.singleType(a) <:< m) Some(a.asInstanceOf[A]) else None
}
object RichCast {
  implicit def optionCast(a: AnyRef) = new RichCast(a)
}
trait Subject {
  private var observers = List[Observer]()
  def addObserver(observer: Observer) = if (!observers.contains(observer)) observers ::= observer
  def removeObserver(observer: Observer) = observers = observers filterNot (_ == observer)
  def notifyObservers = observers foreach (_.receiveUpdate(this))
}
trait Observer {
  def receiveUpdate(subject: Subject)
}
class DoOnce[A](once: ⇒ A)(otherwise: ⇒ A) {
  var done = false
  def get(): A = if (done) otherwise else {
    done = true
    once
  }
  def reset() { done = false }
}
class ResetableLazy[A](calc: ⇒ A) {
  var cached: Option[A] = None
  def apply: A = {
    if (!cached.isDefined) cached = Some(calc)
    cached.get
  }
}
class Cached[A](compute: ⇒ Option[A]) {
  private var value: Option[A] = None
  def apply() = {
    if (value == None) value = compute
    value
  }
  def replace(a: A) { value = Some(a) }
  def reset() { value = None }
}
class Cache[A, B](compute: A ⇒ Option[B]) {
  private var map = Map[A, B]()
  def get(a: A): Option[B] = {
    map.get(a).orElse {
      val computed = compute(a)
      computed foreach { c ⇒ add(a, c) }
      computed
    }
  }
  def add(a: A, b: B) { map += (a -> b) }
  def reset() { map = Map() }
  def values = map.values
}
object Timer {
  private var start: Long = 0L
  private var end: Long = 0L
  def go = {
    start = System.nanoTime
  }
  def stop(str: String) = {
    end = System.nanoTime()
    println(str + "\t>   " + (end - start) / 1000000000.0 + " s")
  }
}