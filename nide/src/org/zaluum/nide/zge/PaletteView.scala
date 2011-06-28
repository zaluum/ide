package org.zaluum.nide.zge
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jface.viewers.ArrayContentProvider
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.{ TreeViewer ⇒ JTreeViewer, Viewer ⇒ JViewer }
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.{ Composite, Control, Label }
import org.eclipse.swt.SWT
import org.eclipse.ui.part.ViewPart
import org.eclipse.ui.{ IPartListener, IWorkbenchPart }
import org.zaluum.nide.eclipse.{ GraphicalEditor, EclipseUtils }
import org.eclipse.jface.viewers.ListViewer
import org.eclipse.ui.part.PageBook
import org.eclipse.jface.viewers.ITreeContentProvider
import org.zaluum.nide.eclipse.BoxTypeProxy
import org.zaluum.nide.eclipse.ZaluumProject
import org.eclipse.swt.graphics.Image
import org.eclipse.jdt.internal.ui.JavaPluginImages
import org.eclipse.swt.dnd.DragSource
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.dnd.DragSourceAdapter
import org.eclipse.swt.dnd.DragSourceEvent
import org.eclipse.jface.viewers.IStructuredSelection

class Page(val zproject: ZaluumProject, comp: Composite) {

  val viewer = new JTreeViewer(comp, SWT.MULTI | SWT.H_SCROLL
    | SWT.V_SCROLL);
  viewer.setContentProvider(provider);
  viewer.setLabelProvider(new LabelProvider() {
    override def getText(element: Object): String = element match {
      case s: String ⇒ s
      case b: BoxTypeProxy ⇒ b.simpleName
    }
    override def getImage(element: Object): Image = element match {
      case s: String ⇒ JavaPluginImages.get(JavaPluginImages.IMG_OBJS_PACKDECL)
      case b: BoxTypeProxy ⇒ null /*viewer.imageFactory(boxProxy.name)*/
    }
  });
  viewer.setInput(Array());
  val ds = new DragSource(viewer.getControl, DND.DROP_MOVE);
  ds.setTransfer(Array(TextTransfer.getInstance()));
  ds.addDragListener(new DragSourceAdapter() {
    def element = viewer.getSelection.asInstanceOf[IStructuredSelection].getFirstElement
    override def dragStart(event: DragSourceEvent) {
      element match {
        case b: BoxTypeProxy ⇒
        case _ ⇒ event.doit = false
      }
    }
    override def dragSetData(event: DragSourceEvent) {
      element match {
        case b: BoxTypeProxy ⇒ event.data = b.name.str
        case _ ⇒ event.doit = false
      }
    }
  });

  def control = viewer.getControl
  def dispose() {
    viewer.getControl.dispose();
  }
  lazy val provider = new ITreeContentProvider {
    def dispose() {}
    def inputChanged(viewer: JViewer, o: Object, newi: Object) {}
    def fetchGrouped() = {
      EclipseUtils.withProgress[Map[String, Seq[BoxTypeProxy]]]("Fetching palette") { monitor ⇒
        zproject.index(monitor).groupBy(_.pkgName)
      }
    }
    var grouped = fetchGrouped()
    def getElements(inputElement: AnyRef): Array[AnyRef] = {
      grouped.keys.toArray
    }

    def getChildren(parentElement: AnyRef): Array[AnyRef] = {
      parentElement match {
        case g: Map[_, _] ⇒ grouped.keys.toArray
        case key: String ⇒
          grouped(key).toArray
        case _ ⇒ Array()
      }
    }

    def getParent(element: AnyRef): AnyRef = {
      element match {
        case key: String ⇒ grouped
        case b: BoxTypeProxy ⇒ b.pkgName
        case _ ⇒ null
      }
    }

    def hasChildren(element: AnyRef): Boolean = {
      element match {
        case g: Map[_, _] ⇒ true
        case key: String ⇒ true
        case _ ⇒ false
      }
    };
  }
}
class PaletteView extends ViewPart {
  var pageBook: PageBook = _
  var map = Map[GraphicalEditor, Page]()
  var jmap = Map[ZaluumProject, Page]()
  var defaultPage: Composite = _
  def createPartControl(parent: Composite) {
    pageBook = new PageBook(parent, SWT.None)
    defaultPage = new Composite(pageBook, SWT.NULL);
    defaultPage.setLayout(new FillLayout());
    val msgLabel = new Label(defaultPage, SWT.LEFT | SWT.TOP | SWT.WRAP);
    msgLabel.setText("Palette");
    pageBook.showPage(defaultPage)
    getSite().getPage().addPartListener(partListener);
  }
  def show(p: Page) {
    pageBook.showPage(p.control)
  }
  override def dispose() {
    getSite().getPage().removePartListener(partListener);
    if (defaultPage != null) {
      defaultPage.dispose();
    }
    for (p ← jmap.values) p.dispose
    map = Map()
    jmap = Map()
    if (pageBook != null)
      pageBook.dispose()
    super.dispose();
  }
  object partListener extends IPartListener {
    def partActivated(part: IWorkbenchPart) {
      part match {
        case g: GraphicalEditor ⇒
          jmap.get(g.zproject) match {
            case Some(p) ⇒
              map += (g -> p)
              show(p)
            case None ⇒
              val newPage = new Page(g.zproject, pageBook)
              map += (g -> newPage)
              jmap += (g.zproject -> newPage)
              show(newPage)
          }
        case _ ⇒
      }
    }
    def partBroughtToTop(part: IWorkbenchPart) {}
    def partClosed(part: IWorkbenchPart) {
      part match {
        case g: GraphicalEditor ⇒
          val page = map(g)
          map -= g
          if (!map.values.exists(_ == page)) {
            jmap -= g.zproject
            pageBook.showPage(defaultPage)
            page.dispose()
          }
        case _ ⇒
      }
    }
    def partDeactivated(part: IWorkbenchPart) {}
    def partOpened(part: IWorkbenchPart) {}
  }
  def setFocus() {
    if (pageBook != null)
      pageBook.setFocus();
  }
}