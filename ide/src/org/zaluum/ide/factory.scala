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
    case model2 : PModel => new ModelEditPartWrite {val model = model2 }
    case cbox : ComposedPBox => new BoxEditPartWrite with ComposedEditPartT {val model = cbox}
    case box : PBox => new BoxEditPartWrite {val model = box }
    case port : PPort => new PortEditPartWrite {val model = port }
    case wire : PWire => new WireEditPartWrite {val model = wire }
  }
}
