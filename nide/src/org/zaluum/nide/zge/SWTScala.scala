package org.zaluum.nide.zge

import org.eclipse.swt.widgets.MenuItem
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.widgets.Menu
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.custom.CCombo
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Widget
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.SWT
import org.eclipse.swt.events.{ SelectionAdapter, SelectionEvent, DisposeListener, DisposeEvent, KeyListener, KeyEvent }
import org.eclipse.swt.widgets.{ Button, Text, Listener, Event }
import org.eclipse.swt.events.ModifyListener
import org.eclipse.swt.events.ModifyEvent
object SWTScala {
  def addReaction(b: { def addSelectionListener(l: SelectionListener) })(r: ⇒ Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(e: SelectionEvent) { r }
    })
  }
  def addModifyReaction(b:{ def addModifyListener(l :  ModifyListener) })(r: => Unit) {
    b.addModifyListener(new ModifyListener {  
      def modifyText(e: ModifyEvent) { r }
    });
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
  def newButton(c:Composite, txt:String="", style:Int=SWT.PUSH, layout:AnyRef=null) = {
    val b = new Button(c,style)
    b.setText(txt)
    b.setLayoutData(layout)
    b
  }
  def newText(c: Composite, txt: String = "", style: Int = SWT.SINGLE, layout: AnyRef = null) = {
    val t = new Text(c, style)
    t.setText(txt)
    t.setLayoutData(layout)
    t
  }
  def newCCombo(c:Composite, txt: String, items:Array[String], style: Int = (SWT.DROP_DOWN | SWT.FLAT | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL), layout:Any = null) = {
    val co = new CCombo(c,style)
    co.setItems(items);
    co.setText(txt)
    co.setLayoutData(layout)
    co
  }
  def newCombo(c:Composite, txt: String, items:Array[String], style: Int = (SWT.DROP_DOWN | SWT.FLAT | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL), layout:Any = null) = {
    val co = new Combo(c,style)
    co.setItems(items);
    co.setText(txt)
    co.setLayoutData(layout)
    co
  }
  def newPopupMenu(s:Shell,target:Control)(body:Menu=>Unit) {
    val menu = new Menu(s,SWT.POP_UP)
    body(menu)
    target.setMenu(menu)
    menu
  }
  def newMenuItem(menu:Menu,txt:String, img:Option[Image]=None)(action : =>Unit) {
    val menuItem = new MenuItem(menu, SWT.PUSH)
    menuItem.setText(txt)
    img foreach { menuItem.setImage(_) }
    addReaction(menuItem)(action)
  }
  def addKeyReleasedReaction(obj:{def addKeyListener(k:KeyListener)}, keyCode:Int)(body: =>Unit) {
    obj.addKeyListener(new KeyListener {
        def keyPressed(e: KeyEvent) {}
        def keyReleased(e: KeyEvent) {
          if (e.keyCode == keyCode) body
        }
      })
  }
}