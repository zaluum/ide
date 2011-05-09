package org.zaluum.nide.zge

import org.eclipse.ui.IEditorSite
import org.eclipse.ui.IViewSite
import org.eclipse.ui.PlatformUI
import org.zaluum.nide.compiler.{Tree,SelectionSubject}
import draw2dConversions._
import org.eclipse.draw2d.{FigureCanvas, FreeformViewport, LightweightSystem}
import org.eclipse.swt.graphics.Cursor
import org.eclipse.swt.widgets.Composite
import scala.collection.JavaConversions._

abstract class Viewer(parent: Composite, val controller: Controller) extends FreeformViewport {
  def shell = parent.getShell
  def display = shell.getDisplay
  val light = new LightweightSystem()
  val canvas = new FigureCanvas(parent, light)
  def focus = parent.setFocus
  def tool: Tool
  def refresh()
  def remapSelection(map :  PartialFunction[SelectionSubject,SelectionSubject]) // move to item?
  def blink(s:SelectionSubject)
  def dispose() {
    canvas.dispose()
    controller.unregisterViewer(this)
  }
  def setStatusMessage(s:String) {
    val wb = PlatformUI.getWorkbench();
    val win = wb.getActiveWorkbenchWindow();
    val page = win.getActivePage();
    val part = page.getActivePart();
    val site = part.getSite();
    val vSite =  site.asInstanceOf[IEditorSite];
    val actionBars =  vSite.getActionBars();
    if( actionBars == null ) return ;
    val statusLineManager = actionBars.getStatusLineManager();
    if( statusLineManager == null ) return ;
    statusLineManager.setMessage( s);
  }
}
