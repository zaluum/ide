package org.zaluum.ide
import org.zaluum.runtime._
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

/**
 * Box Edit Part
 * @author frede
 *
 */
class BoxEditPart(val parent:EditPart, val model: VBox) extends BasePart[VBox,BoxFigure] 
                                with Updater with HelpContext with HighlightPart
                                with XYLayoutPart[PortEditPart]{
  def helpKey = "org.zaluum.box"
  override protected def getModelChildren = new ArrayList(model.ports)
  override def createFigure = new BoxFigure() 
  def highlightFigure = fig.rectangle 
  def parentPart = getParent.asInstanceOf[ModelEditPart]
  
  override protected def refreshVisuals {
    parentPart.setLayoutConstraint(this,
        figure, 
        new Rectangle(model.pos._1,model.pos._2,model.size._1,model.size._2))
    fig.name = model.name
    fig.revalidate()
  }

}
class BoxEditPartWrite(parent:EditPart, model:VBox) extends BoxEditPart(parent,model)
    with DeletablePart {
  def delete = null
  override def resizeCommand(p:PortEditPart, rect:Rectangle)={
    val slot = fig.slotFromPosition(rect.getTopLeft)
    if (!model.slotUsed(slot))
      new Command(){override def execute {println("exec")}}
    else null
      //return new CommandWrap(new ChangeSlotPortCommand(((BoxPortEditPart)child).getPort(),slot));
  }
  
}
/**
 * Wire Edit Part
 * @author frede
 *
 */
class WireEditPart(val model : VWire) extends AbstractConnectionEditPart 
        with BasePart[VWire,PolylineConnection] with Updater with ConnectionPart{
  override def createFigure = WireFigure()
  override def delete = null
  override def refreshVisuals {
    val figureConstraint = new ArrayList[RelativeBendpoint]();
    for (wbp <- model.bendpoints)
    {
      val rbp = new RelativeBendpoint(fig);
      rbp.setRelativeDimensions(
          new Dimension(wbp.a.x, wbp.a.y), 
          new Dimension(wbp.b.x, wbp.b.y))
      rbp.setWeight(0);
      figureConstraint.add(rbp);
    }
    getConnectionFigure().setRoutingConstraint(figureConstraint);
  }
}
        
/**
 * Port Edit Part
 * @author frede
 *
 */
class PortEditPart(val model : VPort)extends BasePart[VPort,PortFigure] 
               with SimpleNodePart with Updater with HelpContext with HighlightPart{
  def helpKey = "org.zaluum.Port"
  def anchor = fig.anchor

  private def filterWires (f : (VWire => Boolean)) = {
    val s = Set[VWire]()
    for {
      box <- Option(model.vbox.parent)
      c <- box.connections
      if f(c)
    } s+= c
    new ArrayList(s)
  }
  override def getModelSourceConnections():JList[_] = filterWires(_.from==model)
  override def getModelTargetConnections():JList[_] = filterWires(_.to==model)
  override def createFigure = new PortFigure
  def highlightFigure = fig.triangle 
  override def refreshVisuals {
    fig.arrange(true,model.slot.left, model.slot.pos, model.name, "link")
  }
}
