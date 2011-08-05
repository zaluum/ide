package org.zaluum.nide.eclipse

import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException

class BuildNotifier(val monitor: IProgressMonitor) {
  val totalWork = 10000
  var work = 0
  def begin() {
    monitor.beginTask("", totalWork)
  }
  def checkCancel() = {
    if (monitor.isCanceled) throw new OperationCanceledException()
  }
  def done() {
    monitor.done()
  }
  def subtask(msg: String) {
    monitor.subTask(msg)
  }
  def update(d: Double) {
    val workDone = (d * 10000).asInstanceOf[Int]
    monitor.worked(workDone)
  }
}