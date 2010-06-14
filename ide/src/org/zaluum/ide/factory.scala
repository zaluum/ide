package org.zaluum.ide
import org.eclipse.gef._
import org.zaluum.runtime._
object ZaluumReadOnlyFactory extends EditPartFactory {
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case model : VModel => new ModelEditPart(model)
    case cbox : ComposedVBox => new BoxEditPart(context,cbox) with ComposedEditPartT
    case box : VBox => new BoxEditPart(context,box)
    case port : VPort => new PortEditPartRead(port)
    case wire : VWire => new WireEditPart(wire)
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
