package org.zaluum.nide.zge

class SelectionManager[T <: CanShowFeedback]{
  var selected = Set[T]()
  def apply(f:T) = selected(f)
  def select(f:T) {
     if (!selected(f)) {
      selected += f
      f.showFeedback
    }
  }
  def deselect(f:T) {
    if (selected(f)) {
      f.hideFeedback
      selected -= f
    }
  }
  def toggleSelection(f:T) {
    if (selected(f)) deselect(f)
    else select(f)
  }
  def deselectAll() { selected foreach (deselect(_))  }
  def updateSelection(figs: Set[T],shift:Boolean){
    if (shift) {
      figs foreach { toggleSelection(_) }
    } else {
      deselectAll()
      figs foreach { select(_) }
    }
  }
}
abstract class ModelViewMapper[M,V <: CanShowUpdate] {
  var viewMap = Map[M,V]()
  def modelSet : Set[M] 
  def buildFigure(m:M):V
  def update() {
    val removed = viewMap.keySet -- modelSet 
    val added = modelSet -- viewMap.keys
    removed foreach { m => viewMap(m).hide; viewMap -= m }
    for (m <- added) {
      val f = buildFigure(m)
      viewMap += (m -> f)
      f.show
    }
    viewMap.values foreach { _.update()}
  }
  def apply(m:M) = viewMap(m)
  def get(m:M) = viewMap.get(m)
}
