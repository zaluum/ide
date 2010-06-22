package org.zaluum.ide
import org.zaluum.runtime.{Command=>C,Bendpoint=>BP,_}
import org.eclipse.swt.SWT
import org.eclipse.draw2d.{PolylineConnection,RelativeBendpoint, FreeformLayer,FreeformLayout, IFigure, ColorConstants}
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
  type T <: VisualModel
  trait VModel extends Subject{
    def currentView:Viewport
    def moveTo(c:Viewport)
    def up()
    def root : T#C
  }
  abstract class ModelEditPart extends BasePart with XYLayoutPart with SnapPart with Subject with Updater{
    type S = VModel
    protected var currentView : Viewport = TopView
    receiveUpdate(model)
    override def getModelChildren : java.util.List[_]= currentView match  {
      case ComposedView(c) => new ArrayList(c.boxes)
      case TopView => Buffer[AnyRef](model.root)
    }
    override def receiveUpdate(s:Subject){
      if (s==model){
        currentView foreach {_.removeObserver(this)}
        currentView = model.currentView
        currentView foreach {_.addObserver(this)}
      }
      super.receiveUpdate(s)
    }
    override def deactivate {
      currentView foreach {_.removeObserver(this)}
      super.deactivate
    }
    override def refreshVisuals  {
      fig.setBackgroundColor(ColorConstants.lightGray)
      fig.setOpaque(currentView == TopView)
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
    type S = T#B
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

  trait ComposedEditPartT extends BoxEditPart with OpenPart{
    def doOpen = {
      val m = parentPart.model
      m.moveTo(ComposedView(model.asInstanceOf[T#C]))
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
    type S = T#W
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
                 with HelpContext with NeighborsHighlightPart
                 with RefPropertySource
                 {
    type F <: PortFigure
    type S = T#P
    def properties = List(
         BooleanProperty("Is input",model.in _, None),
         StringProperty("Type", model.ttype _, None),
         StringProperty("Name", model.name _, None),
         StringProperty("Label", model.link _, None)
         )
    def helpKey = "org.zaluum.Port"
    def anchor = fig.anchor
   
    private def filterWires (f : (T#W => Boolean)) = {
      val s = Set[T#W]()
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
    def highlightNeighbors = {
    	val mainBoxPart = getParent.getParent.asInstanceOf[AbstractGraphicalEditPart]
    	var triangles : Buffer[Triangle] = Buffer()
    	//FIXME: High performance and correct selection (works equal with for-yield structure...)
    	for (b <- mainBoxPart.getChildren) {
      	for(p <- b.asInstanceOf[AbstractGraphicalEditPart].getChildren) {
      		val portm = p.asInstanceOf[AbstractGraphicalEditPart].getModel.asInstanceOf[P]
    	    val portf = p.asInstanceOf[AbstractGraphicalEditPart].getFigure.asInstanceOf[PortFigure].triangle
      		if(portm.link!="" && portm.link == model.link) {
      			triangles.add(portf)
      		}
      	}
      } 
      List()++triangles
    }
    override def refreshVisuals {
      fig.arrange(model.in,model.slot.left, model.slot.pos, model.name, model.link)
    }
  }
}