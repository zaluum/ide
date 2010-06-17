package org.zaluum.ide

import org.eclipse.gef.ui.parts.ContentOutlinePage
import org.eclipse.gef.tools.SelectEditPartTracker
import org.eclipse.swt.widgets.Composite
import org.eclipse.gef.EditPartFactory
import org.eclipse.gef.EditPart
import org.eclipse.gef.editparts.AbstractTreeEditPart
import org.zaluum.runtime.Observer
import org.zaluum.runtime.Subject
import com.google.common.collect.Lists
import java.util.ArrayList
import scala.collection.JavaConversions._
import org.eclipse.gef.Request
import org.eclipse.gef.RequestConstants
import org.eclipse.gef.ui.parts.TreeViewer
import org.zaluum.runtime.PersistentModel._

class ZaluumOutlinePage(boxEditor : ZFileEditor) extends ContentOutlinePage(new TreeViewer()) with Observer {
  override def createControl(parent : Composite) = {
    super.createControl(parent)
    getViewer.setEditDomain(boxEditor.editDomain)
    getViewer.setEditPartFactory(new ZaluumOutlineFactory(boxEditor.model))
    getViewer.setContents(boxEditor.model.root)
    boxEditor.model.root.addObserver(this)
  }
  override def dispose = boxEditor.model.removeObserver(this); super.dispose()
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
  override def createEditPart(c:EditPart, m:Object) =  m match {
    case c : ComposedPBox=> 
      new ListenerAbstractTreePart(m) {
        override def getModelChildren = new ArrayList(c.boxes)
        override def getText = c.name;
        override def getImage = Activator.getDefault.getImageRegistry.get("composed_16")
        override def getDragTracker(req : Request) = new SelectEditPartTracker(this)
        override def performRequest(req : Request) = req.getType match {
          case RequestConstants.REQ_OPEN => if(manager!=null) { /*TODO: Open Selected Part*/ }
          case _ => super.performRequest(req)
        }
      }
    case b: PBox => 
     new ListenerAbstractTreePart(m) {
        override def getModelChildren = new ArrayList()
        override def getText = b.name;
        override def getImage = Activator.getDefault.getImageRegistry.get("composed_16")
        override def getDragTracker(req : Request) = new SelectEditPartTracker(this)
      }
    case _ => error("unexpected box type " + m)
  }
}
