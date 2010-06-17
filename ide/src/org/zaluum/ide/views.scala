package org.zaluum.ide

import org.eclipse.gef.ui.parts.ContentOutlinePage
import org.eclipse.gef.tools.SelectEditPartTracker
import org.eclipse.swt.widgets.Composite
import org.eclipse.gef.EditPartFactory
import org.eclipse.gef.EditPart
import org.eclipse.gef.editparts.AbstractTreeEditPart
import org.zaluum.runtime.PModel
import org.zaluum.runtime.PBox
import org.zaluum.runtime.ComposedPBox
import org.zaluum.runtime.Observer
import org.zaluum.runtime.Subject
import com.google.common.collect.Lists
import java.util.ArrayList
import scala.collection.JavaConversions._
import org.eclipse.gef.Request
import org.eclipse.gef.RequestConstants
import org.eclipse.gef.ui.parts.TreeViewer

class ZaluumOutlinePage(boxEditor : BaseEditor) extends ContentOutlinePage(new TreeViewer()) with Observer {
  val model = boxEditor.model.asInstanceOf[PModel]
  override def createControl(parent : Composite) = {
    super.createControl(parent)
    getViewer.setEditDomain(boxEditor.editDomain)
    getViewer.setEditPartFactory(new ZaluumOutlineFactory(model))
    getViewer.setContents(model.root)
    model.root.addObserver(this)
  }
  override def dispose = model.removeObserver(this); super.dispose()
  override def receiveUpdate(subject:Subject) = getViewer.getContents.refresh
}

class ZaluumOutlineFactory(model : PModel) extends EditPartFactory {
  var manager = model
  class ListenerAbstractTreePart(model : Object) extends AbstractTreeEditPart with Observer {
    setModel(model)
    def getObservable = { getModel.asInstanceOf[Subject] }
    override def activate = {
      if (!isActive) {
        super.activate
        manager.addObserver(this)
        getObservable.addObserver(this)
      }
    }
    override def deactivate = {
      if (isActive) {
        super.deactivate
        getObservable.removeObserver(this)
        manager.removeObserver(this)
      }
    }
    override def receiveUpdate(model : Subject) = refresh
  }
  override def createEditPart(c:EditPart, m:Object) : EditPart = {
    if(m.isInstanceOf[ComposedPBox]) {
      return new ListenerAbstractTreePart(m) {
        override def getModelChildren = new ArrayList(m.asInstanceOf[ComposedPBox].boxes)
        override def getText = m.asInstanceOf[ComposedPBox].name;
        override def getImage = Activator.getDefault.getImageRegistry.get("composed_16")
        override def getDragTracker(req : Request) = new SelectEditPartTracker(this)
        override def performRequest(req : Request) = req.getType match {
          case RequestConstants.REQ_OPEN => if(manager!=null) { /*TODO: Open Selected Part*/ }
          case _ => super.performRequest(req)
        }
      }
    }
    throw new RuntimeException
  }
}
