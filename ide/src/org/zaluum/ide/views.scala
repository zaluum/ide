package org.zaluum.ide

import org.eclipse.gef.ui.parts.ContentOutlinePage
import org.eclipse.gef.ui.parts.TreeViewer
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

class ZaluumOutlinePage(boxEditor : BaseEditor) extends ContentOutlinePage(new TreeViewer()) with Observer {
  
  val model = boxEditor.model.asInstanceOf[PModel]
  override def createControl(parent : Composite) = {
    super.createControl(parent)
    getViewer.setEditDomain(boxEditor.editDomain)
    getViewer.setEditPartFactory(new EditPartFactory {
      override def createEditPart(c:EditPart, m:Object) : EditPart = {
        if(m.isInstanceOf[ComposedPBox]) {
          return new AbstractTreeEditPart(m) {
            override def getModelChildren = new ArrayList(m.asInstanceOf[ComposedPBox].boxes)
            override def getText = m.asInstanceOf[ComposedPBox].name
            override def getImage = Activator.getDefault.getImageRegistry.get("composed_16")
          }
        }
        throw new RuntimeException
      }
    })
    getViewer.setContents(model.root)
    model.root.addObserver(this)
  }
  override def dispose = model.removeObserver(this); super.dispose()
  override def receiveUpdate(subject:Subject) = getViewer.getContents.refresh
}


