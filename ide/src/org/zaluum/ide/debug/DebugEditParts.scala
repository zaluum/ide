package org.zaluum.ide.debug

import org.zaluum.ide._
import org.zaluum.runtime.{Command=>C,Bendpoint=>BP,_}
import org.zaluum.runtime.serial.ModelProtos
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Display
import org.eclipse.core.runtime._
import org.eclipse.gef._
import commands._
import editpolicies._
import rulers._
import requests._
import ui.parts._
import ui.actions._
import palette._
import editparts._
import org.eclipse.jface.resource._
import org.eclipse.ui._
import views.properties._
import org.eclipse.help.IContextProvider
import org.eclipse.swt.graphics._
import scala.collection.JavaConversions._
import scala.collection.mutable._
import scala.collection.immutable.{Set=>ISet}
import java.util.ArrayList
import java.util.{List => JList}
import org.eclipse.draw2d.geometry.{Rectangle,Dimension}
import Commands._
import Debug2Model._
import se.scalablesolutions.akka.actor._
import se.scalablesolutions.akka.dispatch._
import org.zaluum.ide.Utils._
import org.zaluum.runtime._
import java.util.concurrent.TimeUnit


object DebugEditParts extends Parts{
  type T = Debug2Model.type
  
  class DebugModel ( val root : ComposedDBox, val conn : DebugConnection) extends VModel {
    
    @volatile var currentView:Viewport = TopView 
      
    def up() = currentView match {
      case ComposedView(c) =>
        val parent = Option(c.parent.asInstanceOf[ComposedDBox])
        setView(parent)
      case TopView => 
    }
    def moveTo(o:Viewport)  = o match {
      case ComposedView(c:ComposedDBox) => setView(Some(c)) // XXX fix type
      case TopView => setView(None)
    }   
    private def setView(box: Option[ComposedDBox]) = synchronized{
      box match { 
        case Some(b) => currentView = ComposedView(b)
        case None => currentView = TopView
      }
      notifyObservers
    }
  }

  
  class DModelEditPart(val model:DebugModel) extends ModelEditPart {
    override def deactivate {
      model.conn.stop
      super.deactivate
    }
  }
  class DBoxEditPart(val model:DBox) extends BoxEditPart {
    lazy val debugModel = getRoot.getChildren.get(0).asInstanceOf[DModelEditPart].model
    override def activate {
      debugModel.conn.suscribe(model)
      super.activate
    }
    override def deactivate {
      debugModel.conn.unsuscribe(model)
      super.deactivate
    }
  }
  class DPortEditPart(val model:DPort) extends PortEditPart with DirectEditPart {
    type F = PortDebugFigure
    override def createFigure = new PortDebugFigure
    override def editFigure = fig.value
    lazy val debugModel = getRoot.getChildren.get(0).asInstanceOf[DModelEditPart].model

    override def editCommand(v:String) = {
      debugModel.conn.push(model,Integer.parseInt(v))
      null
    }
    override def contents = Array()
    override def refreshVisuals {
      fig.arrange(model.in,model.slot.left, model.slot.pos, model.name, model.link, model.value)
    }
  }
  class DWireEditPart(val model:DWire) extends WireEditPart
  class ComposedDBoxEditPart(val model:DBox) extends BoxEditPart with ComposedEditPartT
}
class PortDebugFigure extends PortFigure {
  override def getFont = anchorFont
  override def getTextToEdit = value.getText
  override def getEditLabel = value
}


