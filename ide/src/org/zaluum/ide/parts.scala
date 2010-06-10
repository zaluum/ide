package org.zaluum.ide
import org.zaluum.runtime.{Command=>C,_}
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
/**
 * 
 * @author frede
 *
 */
object classes{
  val vport = classOf[VPort]
  val ComposedVBoxClass = classOf[ComposedVBox]
}

class ModelEditPart(val model : VModel) extends MainPart[Subject]{
  currentSubject = model
  override def getModelChildren = currentSubject match  {
    case c:ComposedVBox => new ArrayList(c.boxes)
    case v:VModel => Buffer(v.root)
  }

  override def createCommand(t : AnyRef, r:Rectangle) = (t,currentSubject) match {
    case (classes.ComposedVBoxClass, c:ComposedVBox) =>
      val b = new PBox
      b.pos =(r.x,r.y)
      b.size =(r.width,r.height)
      var name = "box"
      var i = 0
      while (c.boxes exists {_.name == "box"+ i}) {i+=1}
      b.name = "box"+i
      new CreateBoxCommand(c,b)
    case _ => null
  }

  def up() = {
    currentSubject match {
      case c: ComposedVBox => currentSubject = c.parent match { 
        case null => model
        case p => p
      }
      case c: VModel => 
    }
  }
  override def refreshVisuals  {
    fig.setBackgroundColor(ColorConstants.lightGray)
    fig.setOpaque(currentSubject.isInstanceOf[VModel])
  }
  

}
/**
 * Box Edit Part
 * @author frede
 *
 */
class BoxEditPart(val parent:EditPart, val model: VBox) extends BasePart[VBox] 
                                with Updater with HelpContext with HighlightPart
                                with XYLayoutPart with RefPropertySource[VBox]{
  type F = BoxFigure
  def helpKey = "org.zaluum.box"
  val properties = List(StringProperty("Name",model.name _,model.name_= _))
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
  def doOpen = parentPart.currentSubject = model
}

class BoxEditPartWrite(parent:EditPart, model:VBox) extends BoxEditPart(parent,model)
    with DeletablePart {
  def delete = DeleteBoxCommand(model)  
  def freeSlot(r:Rectangle) : Option[Slot] = {
    val slot = fig.slotFromPosition(r.getTopLeft)
    if (model.slotUsed(slot)) None else Some(slot)
  }
  override def resizeCommand(r:Resizable, rect:Rectangle)={
    (r,freeSlot(rect)) match { 
      case (p:VPort,Some(slot:Slot)) => new SCommand(p.slot,p.slot_=,slot,p)
      case _ => null
    } 
  }
  override def createCommand(clazz:Object, r:Rectangle) =  {
    (clazz, freeSlot(r)) match {
      case (classes.vport, Some(s:Slot)) => CreatePortCommand(model,s)  
      case _ => null
    }
  }
}
/**
 * Wire Edit Part
 * @author frede
 *
 */
class WireEditPart(val model : VWire) extends AbstractConnectionEditPart 
        with BasePart[VWire] with Updater with ConnectionPart {
  type F = PolylineConnection
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

class WireEditPartWrite(model:VWire) extends WireEditPart(model) {
  def modelEditPart = this.getRoot.
    asInstanceOf[RootEditPart].
    getChildren.get(0).
    asInstanceOf[ModelEditPart] // TODO improve
  override def delete = DeleteWireCommand(
                          model,
                          modelEditPart.currentSubject.asInstanceOf[ComposedVBox])
}
/**
 * Port Edit Part
 * @author frede
 *
 */
class PortEditPart(val model : VPort)extends BasePart[VPort] 
               with SimpleNodePart[VPort] with Updater 
               with HelpContext with HighlightPart
               with RefPropertySource[VPort]
               with DirectEditPart {
  type F = PortFigure
  def helpKey = "org.zaluum.Port"
  def anchor = fig.anchor
  val properties = List(
       BooleanProperty("Is input",model.in _, model.in_= _),
       StringProperty("Type", model.ttype _, model.ttype_= _),
       StringProperty("Name", model.name _, model.name_= _),
       StringProperty("Label", model.link _, model.link_= _)
       )
  def editFigure = fig.link
  def editCommand(v:String) = new SCommand(model.link,model.link_=,v,model)
  def contents = {
    val ports : Buffer[String] = Buffer()
    val mainBox = model.vbox.parent
    for(p <- mainBox.ports)
      if(p.name!="" && !ports.contains(upSeparator + p.name) && model.in!=p.in) ports.add(upSeparator + p.name)
    for(b <- mainBox.boxes)
      for(p <- b.ports)
        if(p.link!="" && !ports.contains(p.link) && model.in!=p.in) ports.add(p.link)
    ports.toArray
  }
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
    fig.arrange(true,model.slot.left, model.slot.pos, model.name, model.link)
  }
}
class PortEditPartWrite(model:VPort) extends PortEditPart(model){
  override def connect(source:VPort) = new CreateWireCommand(model.vbox.parent,source,model)
  override def reconnect(req:ReconnectRequest) = null
}
