package org.zaluum.ide
import org.eclipse.gef._
import org.zaluum.runtime._
import PersistentModel._
object ZaluumReadOnlyFactory extends EditPartFactory {
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case model : VisualModel#VModel => new ModelEditPart(model)
    case cbox : VisualModel#ComposedVBox => new BoxEditPart(context,cbox) with ComposedEditPartT
    case box : VisualModel#VBox => new BoxEditPart(context,box)
    case port : VisualModel#VPort => new PortEditPartRead(port)
    case wire : VisualModel#VWire => new WireEditPart(wire)
    case _ => null
  }
}
object ZaluumWriteFactory extends EditPartFactory {
  def createEditPart(context: EditPart, model: Object): EditPart = model match {
    case model : PModel => new ModelEditPartWrite(model)
    case cbox : ComposedPBox => new BoxEditPartWrite(context,cbox) with ComposedEditPartT
    case box : PBox => new BoxEditPartWrite(context,box)
    case port : PPort => new PortEditPartWrite(port)
    case wire : PWire => new WireEditPartWrite(wire)
  }
}
