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
import org.eclipse.draw2d.geometry.Rectangle

trait BasePart[T<:Subject,F<:Figure] extends AbstractGraphicalEditPart with Observer{
  def model : T
  def fig = getFigure.asInstanceOf[F];
  setModel(model)
  override def activate { 
    super.activate()
    model.addObserver(this)
  }
  override def deactivate { 
    model.removeObserver(this)
    super.deactivate()
  }
  override protected def createEditPolicies (){}
}
trait Updater {
  self : Observer with AbstractGraphicalEditPart =>
  override def receiveUpdate(s: Subject) {
    if (isActive)
      refresh
  }
}
trait HelpContext extends IAdaptable{
  def helpKey:String
  abstract override def getAdapter(key: Class[_]) = {
    if (key == classOf[IContextProvider]) 
      new ContextProvider(helpKey);
    else super.getAdapter(key)
  }
}
trait PropertySource extends IAdaptable{
  def propertySource : IPropertySource
  abstract override def getAdapter(key: Class[_]) = {
    if (key == classOf[IPropertySource]) 
      propertySource
    else super.getAdapter(key)
  }
}
trait HighlightPart extends AbstractGraphicalEditPart {
  def highlightFigure : Shape
  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.SELECTION_FEEDBACK_ROLE, new HighlightEditPolicy(){
      override def containerFigure = highlightFigure 
    });
    super.createEditPolicies
  }
}
trait XYLayoutPart[T<:EditPart] extends AbstractGraphicalEditPart{
  def resizeCommand(child:T, rect:Rectangle):Command = null
  def createCommand(req:CreateRequest):Command = null
  def resizableChild = false

  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.LAYOUT_ROLE, new XYLayoutEditPolicy(){
        override protected def 
          createChangeConstraintCommand(child: EditPart, constraint : Object) :Command = 
            (child,constraint) match 
            {case (c, rect:Rectangle) => resizeCommand(c.asInstanceOf[T],rect)}
        override protected def getCreateCommand(request : CreateRequest) = null
        override protected def createChildEditPolicy(child : EditPart) = 
          if (resizableChild) new NonResizableEditPolicy()
          else super.createChildEditPolicy(child)
      });
    super.createEditPolicies
  }
}
trait DeletablePart extends AbstractGraphicalEditPart{
  def delete : Command
  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.COMPONENT_ROLE, new ComponentEditPolicy() {
      override def createDeleteCommand(deleteRequest:GroupRequest) = delete
    })
    super.createEditPolicies
  }
}
trait SimpleNodePart extends NodeEditPart{
  def anchor : ConnectionAnchor
  override def getSourceConnectionAnchor(connection:ConnectionEditPart)= anchor
  override def getSourceConnectionAnchor(connection:Request)           = anchor
  override def getTargetConnectionAnchor(connection:ConnectionEditPart)= anchor
  override def getTargetConnectionAnchor(connection:Request)           = anchor  
}

trait ConnectionPart extends AbstractConnectionEditPart{
  def delete : Command
  override abstract protected def createEditPolicies{
    //installEditPolicy(EditPolicy.CONNECTION_BENDPOINTS_ROLE,
      //  new LineBendpointEditPolicy());
    installEditPolicy(EditPolicy.CONNECTION_ENDPOINTS_ROLE,
        new ConnectionEndpointEditPolicy());
    installEditPolicy(EditPolicy.CONNECTION_ROLE, new ConnectionEditPolicy(){
      def getDeleteCommand(r: GroupRequest) = delete
    });
    super.createEditPolicies
  }
}