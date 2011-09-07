package org.zaluum.nide.zge
import org.zaluum.nide.eclipse.GraphicalEditor
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.swt.SWT
import org.eclipse.ui.IEditorPart
import org.eclipse.jface.viewers.{ Viewer ⇒ JViewer }
import org.eclipse.jface.viewers.{ TreeViewer ⇒ JTreeViewer }
import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.ArrayContentProvider
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.eclipse.jface.viewers.ListViewer
import org.eclipse.ui.views.properties.PropertySheetViewer
import org.eclipse.ui.views.properties.PropertySheetPage
import org.eclipse.ui.part.PageSite
import org.eclipse.ui.views.properties.IPropertySourceProvider
import org.eclipse.ui.views.properties.IPropertySource
object PropertiesView {
  val ID = "org.zaluum.nide.propertiesView"
}
class PropertiesView extends PageBookView {
  var map = Map[GraphicalEditor, PropertySheetPage]()
  def createDefaultPageContents(defaultPage: Composite) {
    val msgLabel = new Label(defaultPage, SWT.LEFT | SWT.TOP | SWT.WRAP);
    msgLabel.setText("Open a Zaluum Editor to see the available items");

  }
  override def dispose() {
    for (p ← map.values) p.dispose
    map = Map()
    super.dispose()
  }
  def activatePart(part: IWorkbenchPart) {
    part match {
      case g: GraphicalEditor ⇒
        map.get(g) match {
          case Some(p) ⇒
            pageBook.showPage(p.getControl())
          case None ⇒
            val newPage = new PropertySheetPage()
            newPage.init(new PageSite(getViewSite))
            newPage.createControl(pageBook);
            g.addSelectionListener(() ⇒ {
              newPage.selectionChanged(g, g.selection)
            })
            map += (g -> newPage)
            pageBook.showPage(newPage.getControl())
        }
      case e: IEditorPart ⇒ pageBook.showPage(defaultPage)
      case _              ⇒
    }
  }
  def closePart(part: IWorkbenchPart) {
    part match {
      case g: GraphicalEditor ⇒
        map.get(g) match {
          case Some(page) ⇒
            map -= g
            page.dispose()
          case None ⇒
        }
        pageBook.showPage(defaultPage)
      case _ ⇒
    }
  }
}
/*class PropertiesPage(g: GraphicalEditor, view: PropertiesView) {
  lazy val viewer = new PropertySheetViewer(view.pageBook);
  val updateVal: () ⇒ Unit = update _
  viewer.setContentProvider(new ArrayContentProvider());
  def currentSym = g.selected match {
    case Some(i: ValDefItem) ⇒
      i.valSym.tpe match {
        case bs: BoxTypeSymbol ⇒ Some(bs)
        case _                 ⇒ None
      }
    case _ ⇒ None
  }
  def update {
    val props = currentSym match {
      case Some(tpe) ⇒ tpe.beanProperties
      case None      ⇒ List()
    }
    viewer.setInput(props.toArray)
  }
  g.addSelectionListener(updateVal)
  def dispose() {
    g.removeSelectionListener(updateVal)
  }
  def control = viewer.getControl
}
*/ 