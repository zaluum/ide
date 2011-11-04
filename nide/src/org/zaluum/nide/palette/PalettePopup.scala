package org.zaluum.nide.palette

import org.eclipse.jface.window.Window
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.events.ControlAdapter
import org.eclipse.swt.events.ControlEvent
import org.eclipse.swt.events.ShellAdapter
import org.eclipse.swt.events.ShellEvent
import org.eclipse.swt.graphics.Point
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.SWT
import org.zaluum.nide.utils.SWTScala.addOnDispose
import org.zaluum.nide.utils.SWTScala.async
import org.zaluum.nide.utils.Utils.asRunnable

abstract class BasePalettePopup[A](shell: Shell) extends Window(shell) {
  setShellStyle(SWT.TOOL)
  override def getInitialLocation(s: Point) = {
    getShell.getDisplay().getCursorLocation();
  }
  def minWidth: Int
  def closeChildShells() {
    for (s ← getShell().getShells(); if !s.isDisposed()) {
      s.close()
    }
  }
  def activated()
  def deactivated()
  override def configureShell(newshell: Shell) {
    newshell.addShellListener(new ShellAdapter() {
      override def shellDeactivated(e: ShellEvent) {
        deactivated()
      }
      override def shellActivated(e: ShellEvent) {
        child match {
          case Some(c) ⇒
            c.close()
            child = None
          case None ⇒
        }
        activated()
      }
    })
  }
  def fillBar(c: Composite)
  def name: String
  override def createContents(comp: Composite): Control = {
    comp.setLayout {
      val lay = new GridLayout(1, false)
      lay.verticalSpacing = 2
      lay
    }

    val lblPalette = new Label(comp, SWT.NONE);
    lblPalette.setText(name);
    lblPalette.setLayoutData {
      val d = new GridData()
      d.grabExcessHorizontalSpace = true
      d.minimumWidth = minWidth
      d
    }

    val separator = new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
    separator.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));

    val scroll = new ScrolledComposite(comp, SWT.V_SCROLL | SWT.H_SCROLL)
    scroll.setExpandVertical(true)
    scroll.setExpandHorizontal(true)
    val contents = new Composite(scroll, SWT.NONE)
    scroll.setContent(contents)
    scroll.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
    contents.setLayout {
      val g = new GridLayout(1, false)
      g.verticalSpacing = 2
      g.marginBottom = 0
      g
    }

    fillBar(contents)
    scroll.addControlListener(new ControlAdapter() {
      override def controlResized(e: ControlEvent) {
        val r = scroll.getClientArea();
        scroll.setMinSize(contents.computeSize(SWT.DEFAULT,
          SWT.DEFAULT));
      }
    });

    comp
  }

  var child: Option[PalettePopup[A]] = None
  def openhover(k: A) {
    async(getShell.getDisplay) {
      openChild(k)
    }
  }
  def createPopup(key: A): PalettePopup[A]
  protected def openChild(key: A): Unit = {
      def doOpen: PalettePopup[A] = {
        if (getShell.isDisposed()) return null;
        val newp = createPopup(key)
        newp.open()
        if (newp.getShell != null && !newp.getShell().isDisposed()) {
          addOnDispose(newp.getShell) { child = None }
          newp
        } else null
      }
    child match {
      case Some(p) if p.key == key ⇒
      case Some(p) ⇒
        if (p.getShell() != null && !p.getShell().isDisposed())
          p.close()
        child = None
        child = Option(doOpen)
      case None ⇒
        child = Option(doOpen)
    }
  }
}
abstract class FirstPalettePopup[A](shell: Shell) extends BasePalettePopup[A](shell) {
  override def toString = "first"
  var activationId = 0
  def activated() {
    activationId += 1
  }
  def deactivated() {
    val id = activationId
    shell.getDisplay().timerExec(20, asRunnable {
      if (activationId == id && !shell.isDisposed())
        close()
    })
  }
}
abstract class PalettePopup[A](val key: A, val parent: BasePalettePopup[A]) extends BasePalettePopup[A](parent.getShell()) {
  override def activated() { parent.activated() }
  def deactivated() { parent.deactivated() }
  override def toString = key.toString()
}
