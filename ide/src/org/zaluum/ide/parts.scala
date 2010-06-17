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
abstract class Parts{
  type B <: VisualModel#VBox
  type P <: VisualModel#VPort
  type C <: VisualModel#ComposedVBox 
  type W <: VisualModel#VWire
  type M <: VisualModel#VModel
  
  abstract class ModelEditPart extends BasePart with XYLayoutPart with SnapPart with Subject with Updater{
    type S = M
    protected var currentBox : Option[VisualModel#ComposedVBox] = None
    receiveUpdate(model)
    override def getModelChildren : java.util.List[_]= currentBox match  {
      case Some(c:VisualModel#ComposedVBox) => new ArrayList(c.boxes)
      case None => Buffer[AnyRef](model.root)
    }
    override def receiveUpdate(s:Subject){
      if (s==model){
        for (c <- currentBox) 
          c.removeObserver(this)
        currentBox = model.currentBox
        for (c <- currentBox) 
          c.addObserver(this)
      }
      super.receiveUpdate(s)
    }
    override def deactivate {
      for (c <- currentBox)
        c.removeObserver(this)
      super.deactivate
    }
    override def refreshVisuals  {
      fig.setBackgroundColor(ColorConstants.lightGray)
      fig.setOpaque(currentBox == None)
    }
    override def createFigure : IFigure = {
      val freeformLayer = new FreeformLayer()
      freeformLayer.setLayoutManager(new FreeformLayout())
      freeformLayer
    }
  } 
  /**
   * Box Edit Part
   * @author frede
   *
   */
  abstract class BoxEditPart extends BasePart 
                                  with Updater with HelpContext with HighlightPart
                                  with XYLayoutPart with RefPropertySource{
    type F = BoxFigure
    type S = B
    def helpKey = "org.zaluum.box"
    def properties = List(StringProperty("Name",model.name _,None))
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

  trait ComposedEditPartT extends OpenPart{
    self : BoxEditPart =>
    def doOpen = {
      val m = parentPart.model
      m.moveTo(Some(model.asInstanceOf[m.CC]))
    }
  }
  
  /**
   * Wire Edit Part
   * @author frede
   *
   */
  abstract class WireEditPart extends AbstractConnectionEditPart 
          with BasePart with Updater with ConnectionPart {
    type F = PolylineConnection
    type S = W
    override def createFigure = WireFigure()
    
    override def refreshVisuals  = {
      val s = (model.bendpoints map { wbp=>      
            val rbp = new RelativeBendpoint(fig);
            rbp.setRelativeDimensions(
              new Dimension(wbp.a._1, wbp.a._2), 
              new Dimension(wbp.b._1, wbp.b._2))
            rbp.setWeight(0);
            rbp
          }).toBuffer 
      val l = asList(s)
      getConnectionFigure.setRoutingConstraint(l)
    }
  }
  
  
  /**
   * Port Edit Part
   * @author frede
   *
   */
  abstract class PortEditPart extends BasePart
                 with SimpleNodePart with Updater 
                 with HelpContext with HighlightPart
                 with RefPropertySource
                 {
    type F <: PortFigure
    type S = P
    def properties = List(
         BooleanProperty("Is input",model.in _, None),
         StringProperty("Type", model.ttype _, None),
         StringProperty("Name", model.name _, None),
         StringProperty("Label", model.link _, None)
         )
    def helpKey = "org.zaluum.Port"
    def anchor = fig.anchor
   
    private def filterWires (f : (VisualModel#VWire => Boolean)) = {
      val s = Set[VisualModel#VWire]()
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
      fig.arrange(model.in,model.slot.left, model.slot.pos, model.name, model.link)
    }
  }
}