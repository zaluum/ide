package org.zaluum.nide.zge

import org.zaluum.nide.icons.Icons
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.model.BoxClass
import org.eclipse.jface.resource.ImageDescriptor
import org.zaluum.nide.compiler.BoxClassPath
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.swt.widgets.Display


class ImageFactory(val display:Display, bcp: BoxClassPath) {
    val reg = new ImageRegistry
    reg.put("*",ImageDescriptor.createFromFile(classOf[Icons],"notFound.png"))
    def notFound = reg.get("*")
    private def get(resource:String) = {
      Option(reg.get(resource)) orElse {
        val url = bcp.getResource(resource);
        url map { u=>
          reg.put(resource, ImageDescriptor.createFromURL(u))
          reg.get(resource)
        }
      } 
    }
    def apply(boxClass : Option[BoxClass]) : Image= {
      def defaultImage(bc:BoxClass) = bc.className.replace('.','/') + ".png";
      boxClass flatMap { c ⇒ get(c.image).orElse { get(defaultImage(c)) } } getOrElse notFound     
    }
}
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
  def values = viewMap.values
  def apply(m:M) = viewMap(m)
  def get(m:M) = viewMap.get(m)
}
