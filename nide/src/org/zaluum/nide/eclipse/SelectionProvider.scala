package org.zaluum.nide.eclipse

import org.eclipse.jface.viewers.{ IStructuredSelection, IPostSelectionProvider, SelectionChangedEvent, ISelection, ISelectionProvider, StructuredSelection, ISelectionChangedListener }
import org.eclipse.jface.util.SafeRunnable

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