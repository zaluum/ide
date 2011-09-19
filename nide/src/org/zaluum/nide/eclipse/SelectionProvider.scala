package org.zaluum.nide.eclipse

import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IType
import org.eclipse.jface.util.SafeRunnable
import org.eclipse.jface.viewers.IPostSelectionProvider
import org.eclipse.jface.viewers.ISelection
import org.eclipse.jface.viewers.ISelectionChangedListener
import org.eclipse.jface.viewers.ISelectionProvider
import org.eclipse.jface.viewers.SelectionChangedEvent
import org.eclipse.jface.viewers.StructuredSelection
import org.zaluum.nide.zge.Item
import org.eclipse.ui.views.properties.IPropertySource2
import org.eclipse.core.runtime.IAdaptable
import org.zaluum.nide.zge.ValDefItem
import org.zaluum.nide.zge.Controller
import org.eclipse.ui.views.properties.IPropertySource
import org.zaluum.nide.zge.TreeViewer
import org.zaluum.nide.zge.ItemViewer
object SelectionProvider {
  val Jelement = classOf[org.eclipse.jdt.core.IJavaElement]
  val Properties = classOf[IPropertySource]
  def adaptItem(i: Item, controller: Controller): StructuredSelection = {
    new StructuredSelection(new IAdaptable() {
      def getAdapter(cl: Class[_]) = i match {
        case v: ValDefItem if (v.valSym.tpe != null) ⇒
          cl match {
            case Jelement ⇒
              controller.zproject.jProject.findType(v.valSym.tpe.fqName.str)
            case _ ⇒ null
          }
        case _ ⇒ null
      }
    })
  }
  def adaptType(i: IType): StructuredSelection =
    new StructuredSelection(new IAdaptable() {
      def getAdapter(cl: Class[_]) = {
        cl match {
          case Jelement ⇒ i
          case _        ⇒ null
        }
      }
    })
}
class SelectionProvider extends ISelectionProvider with IPostSelectionProvider {
  private var list = List[ISelectionChangedListener]()
  private var post = List[ISelectionChangedListener]()
  private var selection: ISelection = StructuredSelection.EMPTY;
  def addSelectionChangedListener(listener: ISelectionChangedListener) {
    if (!list.contains(listener)) list = listener :: list
  }
  def addPostSelectionChangedListener(listener: ISelectionChangedListener) {
    if (!post.contains(listener)) post = listener :: post
  }
  def removePostSelectionChangedListener(listener: ISelectionChangedListener) {
    post = post filterNot (_ == listener)
  }
  def getSelection = selection
  def removeSelectionChangedListener(listener: ISelectionChangedListener) {
    list = list filterNot (_ == listener)
  }
  def setSelection(selection: ISelection) {
    this.selection = selection
    val event = new SelectionChangedEvent(this, selection);
    for (l ← list.view ++ post) {
      SafeRunnable.run(new SafeRunnable() {
        def run { l.selectionChanged(event) }
      })
    }
  }
}