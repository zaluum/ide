package org.zaluum.nide.zge

import org.eclipse.swt.events.{SelectionAdapter,SelectionEvent}
import org.eclipse.swt.widgets.Button
object SWTScala {
  def addReaction(b:Button)(r : =>Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(e: SelectionEvent) {
        r
      }
    })
  }
}