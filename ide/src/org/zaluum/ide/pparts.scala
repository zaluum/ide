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
import PersistentModel._
object classes{
  val pport = classOf[PPort]
  val ComposedPBoxClass = classOf[ComposedPBox]
}

class PortEditPartWrite(val model :PPort) extends PortEditPart
            with DirectEditPart with RefPropertySourceWrite{
  type S = PPort
   def properties = List(
       BooleanProperty("Is input",model.in _, Some(model.in_= _)),
       StringProperty("Type", model.ttype _, Some(model.ttype_= _)),
       StringProperty("Name", model.name _, Some(model.uniqueName _)),
       StringProperty("Label", model.link _, Some(model.link_= _))
       )
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
class BoxEditPartWrite(parent:EditPart, model:PBox) extends BoxEditPart(parent,model)
    with DeletablePart with RefPropertySourceWrite{
  override def properties = List(StringProperty("Name",model.name _,Some(model.uniqueName _)))
  def delete = DeleteBoxCommand(model)  
  def freeSlot(r:Rectangle) : Option[Slot] = {
    val slot = fig.slotFromPosition(r.getTopLeft)
    if (model.slotUsed(slot)) None else Some(slot)
  }
  override def specialPlaceCommand(port:AnyRef, r:Rectangle)={
    (port,freeSlot(r)) match { 
      case (p:PPort,Some(slot)) =>  new SCommand(p.slot,p.slot_=,slot,p)
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