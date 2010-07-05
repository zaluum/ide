package org.zaluum.ide
import org.eclipse.gef._
import org.zaluum.runtime._
import PersistentModel._

object ZaluumWriteFactory extends EditPartFactory {
  import PersistentEditParts._
  def createEditPart(context: EditPart, model: Object): EditPart = model match {
    case model2 : PModel => new ModelEditPartWrite(model2)
    case cbox : ComposedPBox => new BoxEditPartWrite(cbox) with ComposedEditPartT
    case pbox : PlotPBox => new BoxEditPartWrite(pbox) with PlotEditPartT
    case box : PBox => new BoxEditPartWrite(box)
    case port : PPort => new PortEditPartWrite(port)
    case wire : PWire => new WireEditPartWrite(wire)
  }
}
