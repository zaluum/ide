package org.zaluum.ide

import org.zaluum.runtime.{Command=>C,Bendpoint=>BP,_}
import org.eclipse.swt.SWT
import org.eclipse.draw2d._
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
import java.util.ArrayList
import java.util.{List => JList}
import org.eclipse.draw2d.geometry.{Rectangle,Dimension}
import Commands._
import Debug2Model._
class LocalDebugModel(p:MainBox) extends VModel{
  def root : ComposedDBox = p.cdebug
}

object DebugEditParts extends Parts{
  type B = DBox
  type P = DPort
  type W = DWire
  type C = ComposedDBox
  type M = LocalDebugModel
  class DModelEditPart(val model:LocalDebugModel) extends ModelEditPart 
  class DBoxEditPart(val model:DBox) extends BoxEditPart
  class DPortEditPart(val model:DPort) extends PortEditPart{
    type F = PortDebugFigure
    override def createFigure = new PortDebugFigure
    override def refreshVisuals {
      fig.arrange(model.in,model.slot.left, model.slot.pos, model.name, model.link + " = " + model.value)
    }
  }
  class DWireEditPart(val model:DWire) extends WireEditPart
  class ComposedDBoxEditPart(val model:DBox) extends BoxEditPart with ComposedEditPartT
}
class PortDebugFigure extends PortFigure {
}