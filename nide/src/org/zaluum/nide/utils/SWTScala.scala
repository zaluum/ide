package org.zaluum.nide.utils

import org.eclipse.swt.custom.CCombo
import org.eclipse.swt.events.DisposeEvent
import org.eclipse.swt.events.DisposeListener
import org.eclipse.swt.events.KeyEvent
import org.eclipse.swt.events.KeyListener
import org.eclipse.swt.events.ModifyEvent
import org.eclipse.swt.events.ModifyListener
import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.widgets.Combo
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Menu
import org.eclipse.swt.widgets.MenuItem
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.events.MouseTrackAdapter
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.widgets.Canvas
import org.eclipse.swt.events.PaintListener
import org.eclipse.swt.events.PaintEvent
import org.eclipse.swt.events.MouseListener
import org.eclipse.swt.events.MouseAdapter
import org.eclipse.swt.events.MouseTrackListener
import org.zaluum.nide.palette.CustomExpand
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.graphics.Rectangle
object SWTScala {
  def async(display: Display)(body: ⇒ Unit) {
    display.asyncExec(new Runnable() {
      def run {
        body
      }
    })
  }
  def async(body: ⇒ Unit) {
    async(Display.getCurrent)(body _)
  }
  def addTextReaction(b: Text)(r: ⇒ Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetDefaultSelected(e: SelectionEvent) { r }
    })
  }
  def addReaction(b: { def addSelectionListener(l: SelectionListener) })(r: ⇒ Unit) {
    b.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(e: SelectionEvent) { r }
    })
  }
  def addModifyReaction(b: { def addModifyListener(l: ModifyListener) })(r: ⇒ Unit) {
    b.addModifyListener(new ModifyListener {
      def modifyText(e: ModifyEvent) { r }
    });
  }
  def addOnDispose(c: { def addDisposeListener(l: DisposeListener) })(r: ⇒ Unit) {
    c.addDisposeListener(new DisposeListener() {
      def widgetDisposed(e: DisposeEvent) { r }
    })
  }
  def addOnCustomHover(c: Control, ms: Int)(body: ⇒ Unit) = {
    var id: Int = 0
    c.addMouseTrackListener(new MouseTrackAdapter {
      override def mouseEnter(e: MouseEvent) {
        val initId = id
        c.getDisplay.timerExec(ms, new Runnable {
          def run {
            if (id == initId && !c.isDisposed())
              body
          }
        })
      }
      override def mouseExit(e: MouseEvent) {
        id = id + 1
      }
    })
  }
  def addOnEnter(c: Control)(body: ⇒ Unit) = {
    c.addMouseTrackListener(new MouseTrackAdapter {
      override def mouseEnter(e: MouseEvent) {
        if (!c.isDisposed()) {
          body
        }
      }
    })
  }
  def addOnExit(c: Control)(body: ⇒ Unit) = {
    c.addMouseTrackListener(new MouseTrackAdapter {
      override def mouseExit(e: MouseEvent) {
        if (!c.isDisposed()) {
          body
        }
      }
    })
  }
  /*def addReaction(t:Widget)(r : =>Unit) {
    t.addListener(SWT.DefaultSelection, new Listener() {
      override def handleEvent(e: Event) {r}
    })
  }*/
  def newCustomExpand(comp: Composite, text: String)(body: Composite ⇒ Unit): CustomExpand = {
    val custom = new CustomExpand(comp, text)
    custom.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1))
    body(custom.c)
    custom
  }

  def newLabel(c: Composite, txt: String = "", style: Int = SWT.NONE, layout: AnyRef = null) = {
    val l = new Label(c, style)
    l.setText(txt)
    l.setLayoutData(layout)
    l
  }
  def newImageButton(c: Composite, img: Image, txt: String)(reaction: ⇒ Unit) = {
    val canvas = new Canvas(c, SWT.NO_REDRAW_RESIZE)
    canvas.setToolTipText(txt)
    var over = false
    var pressed = false
    canvas.addPaintListener(new PaintListener {
      def paintControl(e: PaintEvent) {
        val gc = e.gc
        val iw = img.getBounds.width
        val ih = img.getBounds.height
        val cw = canvas.getBounds.width
        val ch = canvas.getBounds.height
        val x = if (iw < cw) (cw - iw) / 2 else 0
        val y = if (ih < ch) (ch - ih) / 2 else 0
        gc.drawImage(img, x, y)
        gc.setForeground(gc.getDevice.getSystemColor(SWT.COLOR_GRAY))
        val rect = new Rectangle(0, 0, cw - 1, ch - 1)
        gc.drawRectangle(rect)
        if (over) {
          gc.setAlpha(20)
          gc.setBackground(gc.getDevice().getSystemColor(SWT.COLOR_BLUE))
          gc.fillRectangle(rect)
        }
        //if (pressed) e.gc.drawRectangle(img.getBounds)
      }
    })
    canvas.addMouseTrackListener(new MouseTrackListener {
      def mouseEnter(e: MouseEvent) {
        over = true
        canvas.redraw
      }
      def mouseExit(e: MouseEvent) {
        over = false
        canvas.redraw
      }
      def mouseHover(e: MouseEvent) {}
    })
    canvas.addMouseListener(new MouseAdapter() {
      override def mouseDown(e: MouseEvent) {
        pressed = true
        canvas.redraw()
      }
      override def mouseUp(e: MouseEvent) {
        pressed = false
        canvas.redraw()
        reaction
      }
    })
    canvas
  }
  def newButton(c: Composite, txt: String = "", style: Int = SWT.PUSH, layout: AnyRef = null) = {
    val b = new org.eclipse.swt.widgets.Button(c, style)
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
  def newCCombo(c: Composite, txt: String, items: Array[String], style: Int = (SWT.DROP_DOWN | SWT.FLAT | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL), layout: Any = null) = {
    val co = new CCombo(c, style)
    co.setItems(items);
    co.setText(txt)
    co.setLayoutData(layout)
    co
  }
  def newCombo(c: Composite, txt: String, items: Array[String], style: Int = (SWT.DROP_DOWN | SWT.FLAT | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL), layout: Any = null) = {
    val co = new Combo(c, style)
    co.setItems(items);
    co.setText(txt)
    co.setLayoutData(layout)
    co
  }
  def newPopupMenu(s: Shell, target: Control)(body: Menu ⇒ Unit) {
    val menu = new Menu(s, SWT.POP_UP)
    body(menu)
    target.setMenu(menu)
    menu
  }
  def newMenuItem(menu: Menu, txt: String, img: Option[Image] = None)(action: ⇒ Unit) {
    val menuItem = new MenuItem(menu, SWT.PUSH)
    menuItem.setText(txt)
    img foreach { menuItem.setImage(_) }
    addReaction(menuItem)(action)
  }
  def addKeyReleasedReaction(obj: { def addKeyListener(k: KeyListener) }, keyCode: Int)(body: ⇒ Unit) {
    obj.addKeyListener(new KeyListener {
      def keyPressed(e: KeyEvent) {}
      def keyReleased(e: KeyEvent) {
        if (e.keyCode == keyCode) body
      }
    })
  }
}