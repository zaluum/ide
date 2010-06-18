package org.zaluum.ide
import org.eclipse.gef._
import org.zaluum.runtime._
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