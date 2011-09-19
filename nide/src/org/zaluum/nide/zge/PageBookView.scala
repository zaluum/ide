package org.zaluum.nide.zge

import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.SWT
import org.eclipse.ui.part.PageBook
import org.eclipse.ui.part.ViewPart
import org.eclipse.ui.IPartListener
import org.eclipse.ui.IWorkbenchPart
import org.zaluum.nide.eclipse.SelectionProvider
import org.zaluum.nide.eclipse.GraphicalEditor
import org.eclipse.jface.util.IPropertyChangeListener
import org.eclipse.jdt.launching.PropertyChangeEvent
/* Original PageRec not accessible */
abstract class PageBookView extends ViewPart {
  var pageBook: PageBook = _
  var defaultPage: Composite = _
  lazy val selectionProvider = new SelectionProvider()
  def createPartControl(parent: Composite) {
    pageBook = new PageBook(parent, SWT.None)
    defaultPage = new Composite(pageBook, SWT.NULL);
    defaultPage.setLayout(new FillLayout());
    createDefaultPageContents(defaultPage);
    val page = getSite().getPage()
    if (page != null)
      activatePart(page.getActiveEditor())
    else
      pageBook.showPage(defaultPage)
    page.addPartListener(partListener);
    getViewSite.setSelectionProvider(selectionProvider);
  }
  def createDefaultPageContents(defaultPage: Composite)
  override def dispose() {
    getSite().getPage().removePartListener(partListener);
    if (defaultPage != null)
      defaultPage.dispose();
    if (pageBook != null)
      pageBook.dispose()
    super.dispose();
  }
  def setFocus() {
    if (pageBook != null)
      pageBook.setFocus();
  }
  def activatePart(part: IWorkbenchPart): Unit
  def closePart(part: IWorkbenchPart)
  object partListener extends IPartListener {
    def partActivated(part: IWorkbenchPart) { activatePart(part) }
    def partBroughtToTop(part: IWorkbenchPart) {}
    def partClosed(part: IWorkbenchPart) { closePart(part) }
    def partDeactivated(part: IWorkbenchPart) {}
    def partOpened(part: IWorkbenchPart) {}
  }

}
