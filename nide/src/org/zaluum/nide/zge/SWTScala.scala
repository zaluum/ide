package org.zaluum.nide.zge

import org.eclipse.swt.custom.CCombo
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Widget
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{ SelectionAdapter, SelectionEvent, DisposeListener, DisposeEvent }
import org.eclipse.swt.widgets.{ Button, Text, Listener, Event }
object SWTScala {
  def addReaction(b: { def addSelectionListener(l: SelectionListener) })(r: ⇒ Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(e: SelectionEvent) { r }
    })
  }
  def addOnDispose(c: { def addDisposeListener(l: DisposeListener) })(r: ⇒ Unit) {
    c.addDisposeListener(new DisposeListener() {
      def widgetDisposed(e: DisposeEvent) { r }
    })
  }
  /*def addReaction(t:Widget)(r : =>Unit) {
    t.addListener(SWT.DefaultSelection, new Listener() {
      override def handleEvent(e: Event) {r}
    })
  }*/
  def newLabel(c: Composite, txt: String = "", style: Int = SWT.NONE, layout: AnyRef = null) = {
    val l = new Label(c, style)
    l.setText(txt)
    l.setLayoutData(layout)
    l
  }
  def newText(c: Composite, txt: String = "", style: Int = SWT.SINGLE, layout: AnyRef = null) = {
    val t = new Text(c, style)
    t.setText(txt)
    t.setLayoutData(layout)
    t
  }
  def newCombo(c:Composite, txt: String, items:Array[String], style: Int = (SWT.DROP_DOWN | SWT.FLAT | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL), layout:Any = null) = {
    val co = new CCombo(c,style)
    co.setItems(items);
    co.setText(txt)
    co.setLayoutData(layout)
    co
  }
}