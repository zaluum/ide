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
object SelectionProvider {
  def adaptType(i: IType): StructuredSelection = {
    val El = classOf[org.eclipse.jdt.core.IJavaElement]
    val adaptable = new org.eclipse.core.runtime.IAdaptable() {
      def getAdapter(cl: Class[_]) = {
        cl match {
          case El ⇒ i
          case _  ⇒ null
        }
      }
    }
    new StructuredSelection(adaptable)
  }
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