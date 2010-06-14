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
/**
 * 
 * @author frede
 *
 */
object classes{
  val pport = classOf[PPort]
  val ComposedPBoxClass = classOf[ComposedPBox]
}

class ModelEditPart(val model:BaseVModel) extends MainPart{
  type C <: ComposedVBox
  type S = BaseVModel
  currentSubject = model
  override def getModelChildren : java.util.List[_]= currentSubject match  {
    case c:ComposedVBox => new ArrayList(c.boxes)
    case v:BaseVModel => Buffer(v.root)
  }

  def up() = {
    currentSubject match {
      case c: ComposedVBox => currentSubject = c.parent match { 
        case null =>   model
        case p =>  p.asInstanceOf[C]
      }
      case _ => // do not move 
    }
  }
  override def refreshVisuals  {
    fig.setBackgroundColor(ColorConstants.lightGray)
    fig.setOpaque(currentSubject.isInstanceOf[BaseVModel])
  }
} 
class ModelEditPartWrite(override val model:PModel) extends ModelEditPart(model){
  type C = ComposedPBox
  override def createCommand(t : AnyRef, r:Rectangle) = (t,currentSubject) match {
    case (classes.ComposedPBoxClass, c:ComposedPBox) =>
      val b = new ComposedPBox
      b.pos =(r.x,r.y)
      b.size =(r.width,r.height)
      new CreateBoxCommand(c,b)
    case _ => null
  }
}
/**
 * Box Edit Part
 * @author frede
 *
 */
class BoxEditPart(val parent:EditPart, val model : VBox) extends BasePart 
                                with Updater with HelpContext with HighlightPart
                                with XYLayoutPart with RefPropertySource{
  type F = BoxFigure
  type S = VBox
  def helpKey = "org.zaluum.box"
  def properties = List(StringProperty("Name",model.name _,model.uniqueName _))
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
  def doOpen = parentPart.currentSubject = model.asInstanceOf[S]
}

class BoxEditPartWrite(parent:EditPart, model:PBox) extends BoxEditPart(parent,model)
    with DeletablePart with RefPropertySourceWrite{
  def delete = DeleteBoxCommand(model)  
  def freeSlot(r:Rectangle) : Option[Slot] = {
    val slot = fig.slotFromPosition(r.getTopLeft)
    if (model.slotUsed(slot)) None else Some(slot)
  }
  override def specialPlaceCommand(port:AnyRef, r:Rectangle)={
    (port,freeSlot(r)) match { 
      case (p:VPort,Some(slot)) =>  new SCommand(p.slot,p.slot_=,slot,p)
      case (p,v) => null
    } 
  }
  override def createCommand(clazz:Object, r:Rectangle) =  {
    (clazz, freeSlot(r)) match {
      case (classes.pport, Some(s:Slot)) => CreatePortCommand(model,s)  
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
        with BasePart with Updater with ConnectionPart {
  type F = PolylineConnection
  type S = VWire
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

class WireEditPartWrite(model:PWire) extends WireEditPart(model) {
  def modelEditPart = this.getRoot.
    asInstanceOf[RootEditPart].
    getChildren.get(0).
    asInstanceOf[ModelEditPart] // TODO improve
  override def delete = DeleteWireCommand(
                          model,
                          modelEditPart.currentSubject.asInstanceOf[ComposedPBox])
  implicit def dimToTuple (d:Dimension) = (d.width,d.height)
  implicit def toBendpoint(p : geometry.Point) = {
    fig.translateToRelative(p);
    val ref1 = fig.getSourceAnchor.getReferencePoint
    val ref2 = fig.getTargetAnchor.getReferencePoint
    fig.translateToRelative(ref1);
    fig.translateToRelative(ref2);
    BP(p.getDifference(ref1),p.getDifference(ref2))
  }
  override def createBendpoint(l:geometry.Point, i: Int) = CreateBendpointCommand(model, l, i)
  override def deleteBendpoint(i: Int) = DeleteBendpointCommand(model, i)
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
  type F = PortFigure
  type S <: VPort
  def helpKey = "org.zaluum.Port"
  def anchor = fig.anchor
  def properties = List(
       BooleanProperty("Is input",model.in _, model.in_= _),
       StringProperty("Type", model.ttype _, model.ttype_= _),
       StringProperty("Name", model.name _, model.uniqueName _),
       StringProperty("Label", model.link _, model.link_= _)
       )
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
class PortEditPartRead(val model:VPort) extends PortEditPart{
  type S = VPort
}
class PortEditPartWrite(val model :PPort) extends PortEditPart
            with DirectEditPart with RefPropertySourceWrite{
  type S = PPort
  override def editFigure = fig.link
  override def editCommand(v:String) = new SCommand(model.link,model.link_=,v,model)
  def contents = {
   val mainBox = model.vbox.parent
   val ports = for {
     p <- mainBox.ports
     if(p.name!="" && model.in!=p.in)}  
     yield "@" + p.name
   val labels = for {
     b <- mainBox.boxes
     p <- b.ports
     if(p.link!="" && model.in!=p.in)} 
     yield p.link
   ((List()++labels).sorted ++ ((List()++ports).sorted)).toArray // XXX improve!
  }
  override def connect(source:PPort) = model.vbox.parent match {
    case null=> null // do not create wire on top
    case p => CreateWireCommand(p,source,model)
  }
  override def reconnect(req:ReconnectRequest) = req.getConnectionEditPart.getModel match {
    case w : PWire => 
      val cn = if (req.isMovingStartAnchor) (model,w.to) else (w.from,model) 
      ComposedCommand(List(
        DeleteWireCommand(w,model.vbox.parent),
        CreateWireCommand(model.vbox.parent, cn._1, cn._2)
        ))
    case _ => null
  }
}
