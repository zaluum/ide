package org.zaluum.nide.palette

import java.lang.Object

import org.eclipse.jdt.internal.ui.JavaPluginImages
import org.eclipse.jface.viewers.ColumnLabelProvider
import org.eclipse.jface.viewers.ColumnViewerToolTipSupport
import org.eclipse.jface.viewers.ISelectionChangedListener
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.SelectionChangedEvent
import org.eclipse.jface.viewers.StructuredSelection
import org.eclipse.jface.viewers.{ TreeViewer ⇒ JTreeViewer }
import org.eclipse.jface.viewers.TreeViewerColumn
import org.eclipse.jface.viewers.{ Viewer ⇒ JViewer }
import org.eclipse.jface.viewers.ViewerSorter
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DragSource
import org.eclipse.swt.dnd.DragSourceAdapter
import org.eclipse.swt.dnd.DragSourceEvent
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Label
import org.eclipse.swt.SWT
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.IWorkbenchPart
import org.zaluum.nide.compiler.PortDir
import org.zaluum.nide.eclipse.GraphicalEditor
import org.zaluum.nide.images.ImageFactory
import org.zaluum.nide.eclipse.PageBookView
import org.zaluum.nide.eclipse.SelectionProvider
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.utils.SWTScala.async

object PaletteView {
  val ID = "org.zaluum.nide.paletteView"
}
class PaletteView extends PageBookView {
  var map = Map[GraphicalEditor, PalettePage]()
  var jmap = Map[ZaluumProject, PalettePage]()
  def createDefaultPageContents(defaultPage: Composite) {
    val msgLabel = new Label(defaultPage, SWT.LEFT | SWT.TOP | SWT.WRAP);
    msgLabel.setText("Open a Zaluum Editor to see the available items");

  }
  def show(p: PalettePage) = pageBook.showPage(p.control)
  override def dispose() {
    for (p ← jmap.values) p.dispose
    map = Map()
    jmap = Map()
    super.dispose()
  }
  def activatePart(part: IWorkbenchPart) {
    part match {
      case g: GraphicalEditor ⇒
        jmap.get(g.zproject) match {
          case Some(p) ⇒
            map += (g -> p)
            show(p)
          case None ⇒
            val newPage = new PalettePage(g.zproject, this)
            map += (g -> newPage)
            jmap += (g.zproject -> newPage)
            show(newPage)
        }
      case e: IEditorPart ⇒ pageBook.showPage(defaultPage)
      case _              ⇒
    }
  }
  def closePart(part: IWorkbenchPart) {
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
}

/* PAGE */
class PalettePage(val zproject: ZaluumProject, paletteView: PaletteView) extends PalettePageDND with PalettePageCoreListener {
  lazy val viewer = new JTreeViewer(paletteView.pageBook, SWT.H_SCROLL | SWT.V_SCROLL);
  implicit def display = viewer.getControl.getDisplay
  def control = viewer.getControl
  val imgFactory = new ImageFactory(zproject.projectImageFactory, viewer.getControl)
  val listener: () ⇒ Unit = () ⇒ {
    async(display) {
      viewer.setInput(zproject.palette)
      viewer.refresh()
    }
  }
  viewer.setContentProvider(new PaletteFolderProvider());
  {
    val a = new TreeViewerColumn(viewer, SWT.LEFT)
    a.setLabelProvider(new ColumnLabelProvider() {
      override def getImage(a: AnyRef) = image(a)
      override def getText(a: AnyRef) = text(a)
      override def getToolTipImage(a: AnyRef) = image(a)
      override def getToolTipText(a: AnyRef) = text(a)
    })
    a.getColumn.setWidth(250)
    ColumnViewerToolTipSupport.enableFor(viewer);
  }
  def text(element: Object): String = element match {
    case s: String       ⇒ s
    case b: PaletteEntry ⇒ b.name
  }
  def image(element: Object): Image = element match {
    case f: String       ⇒ JavaPluginImages.get(JavaPluginImages.IMG_OBJS_PACKDECL)
    case b: PaletteEntry ⇒ imgFactory.iconForPalette(b) // XXX destroy // TODO static images
  }
  viewer.setSorter(new ViewerSorter())
  viewer.setInput(zproject.palette);
  viewer.addPostSelectionChangedListener(new ISelectionChangedListener {
    def selectionChanged(event: SelectionChangedEvent) {
      val s = event.getSelection.asInstanceOf[IStructuredSelection]
      s.getFirstElement match {
        case b: PaletteEntry ⇒
          try {
            val t = zproject.jProject.findType(b.className.str);
            paletteView.selectionProvider.setSelection(SelectionProvider.adaptType(t))
          } catch { case e ⇒ }
        case _ ⇒ paletteView.selectionProvider.setSelection(StructuredSelection.EMPTY)
      }
    }
  });
  zproject.loader.addListener(listener)
  // Methods 

  def dispose() {
    zproject.loader.removeListener(listener)
    control.dispose();
    imgFactory.destroyAll()
  }

}
trait PalettePageDND {
  self: PalettePage ⇒
  val ds = new DragSource(viewer.getControl, DND.DROP_MOVE);
  ds.setTransfer(Array(PaletteTransfer));
  ds.addDragListener(new DragSourceAdapter() {
    def element = viewer.getSelection.asInstanceOf[IStructuredSelection].getFirstElement
    override def dragStart(event: DragSourceEvent) {
      element match {
        case b: PaletteEntry ⇒
        case p: PortDir      ⇒
        case _ ⇒
          event.doit = false
      }
    }
    override def dragSetData(event: DragSourceEvent) {
      element match {
        case e: PaletteEntry ⇒ event.data = e
        case _ ⇒
          event.doit = false
      }
    }
  });

}
trait PalettePageCoreListener {
  self: PalettePage ⇒

}
class PaletteFolderProvider extends ITreeContentProvider {
  def dispose() {}
  var opalette: Option[Palette] = _
  def inputChanged(viewer: JViewer, o: Object, newi: Object) {
    opalette = newi.asInstanceOf[Option[Palette]]
  }

  def getElements(inputElement: AnyRef): Array[AnyRef] =
    inputElement match {
      case p: Option[_] ⇒
        val l = p.toList.flatMap(_.asInstanceOf[Palette].map.keys)
        l.toArray.asInstanceOf[Array[AnyRef]]
      case _ ⇒ Array()
    }

  def getChildren(parentElement: AnyRef): Array[AnyRef] = {
    parentElement match {
      case pkg: String ⇒
        opalette.get.map.get(pkg) match {
          case Some(l) ⇒ l.toArray.asInstanceOf[Array[AnyRef]]
          case None    ⇒ Array()
        }
      case _ ⇒ Array()
    }
  }

  def getParent(element: AnyRef): AnyRef = {
    element match {
      case b: PaletteEntry ⇒ b.pkg
      case _               ⇒ null
    }
  }

  def hasChildren(element: AnyRef): Boolean = {
    element match {
      // case p: Palette  ⇒ true
      case pkg: String ⇒ true
      case _           ⇒ false
    }
  };
}
