package org.zaluum.nide.zge
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{SelectionAdapter, SelectionEvent}
import org.eclipse.swt.widgets.{Button, Text, Listener, Event}
object SWTScala {
  def addReaction(b:Button)(r : =>Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(e: SelectionEvent) {
        r
      }
    })
  }
  def addReaction(t:Text)(r : =>Unit) {
    t.addListener(SWT.DefaultSelection, new Listener() {
      override def handleEvent(e: Event) {
        r
      }
    })
  }
}