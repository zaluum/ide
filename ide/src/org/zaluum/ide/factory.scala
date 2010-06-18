package org.zaluum.ide
import org.eclipse.gef._
import org.zaluum.runtime._
object ZaluumReadOnlyFactory extends EditPartFactory {
  import REditParts._
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case model2 : VisualModel#VModel => new ModelEditPart { val model = model2}
    case cbox : VisualModel#ComposedVBox => new BoxEditPart with ComposedEditPartT  {val model=cbox}
    case box : VisualModel#VBox => new BoxEditPart {val model = box}
    case port : VisualModel#VPort => new PortEditPart {val model = port}
    case wire : VisualModel#VWire => new WireEditPart {val model = wire}
    case _ => null
  }
}
import PersistentModel._
object ZaluumWriteFactory extends EditPartFactory {
  import PersistentEditParts._
  def createEditPart(context: EditPart, model: Object): EditPart = model match {
    case model2 : PModel => new ModelEditPartWrite(model2)
    case cbox : ComposedPBox => new BoxEditPartWrite(cbox) with ComposedEditPartT
    case box : PBox => new BoxEditPartWrite(box)
    case port : PPort => new PortEditPartWrite(port)
    case wire : PWire => new WireEditPartWrite(wire)
  }
}
import Debug2Model._
object ZaluumDebugFactory extends EditPartFactory {
  import DebugEditParts._
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case model2 : ModelUpdater => new DModelEditPart(model2)
    case cbox : ComposedDBox => new DBoxEditPart(cbox) with ComposedEditPartT 
    case box : DBox => new DBoxEditPart(box)
    case port : DPort => new DPortEditPart(port)
    case wire : DWire => new DWireEditPart(wire)
    case _ => null
  }
}