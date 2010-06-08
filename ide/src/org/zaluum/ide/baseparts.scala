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

trait DirectEditPart extends AbstractGraphicalEditPart {
  def editFigure : BoxLabel
  def doEdit
  override def performRequest(req : Request) = req.getType match {
    case RequestConstants.REQ_DIRECT_EDIT => doEdit
    case _ => super.performRequest(req)
  }
  def policyEdit
  override abstract protected def createEditPolicies {
    policyEdit
    super.createEditPolicies
  }
}

trait OpenPart extends AbstractGraphicalEditPart {
  def doOpen
  override def performRequest(req : Request) =  req.getType match {
    case RequestConstants.REQ_OPEN => doOpen
    case _ =>  super.performRequest(req)
  }
}

trait MainPart[M <: Subject] extends AbstractGraphicalEditPart with BasePart[VModel,FreeformLayer] with XYLayoutPart with SnapPart with Subject with Updater{
  private var currentSubject_ : M = _
  def currentSubject = currentSubject_
  def currentSubject_= (s:M) {
    if (currentSubject_ ne null)
      currentSubject_.removeObserver(this);
    currentSubject_ = s;
    currentSubject_.addObserver(this);
    notifyObservers
    if (isActive)
      refresh();
  }
  override def deactivate = {
    if (currentSubject_ != null)
      currentSubject_.removeObserver(this);
    super.deactivate();
  }
  override def createFigure : IFigure = {
    val freeformLayer = new FreeformLayer()
    freeformLayer.setLayoutManager(new FreeformLayout())
    freeformLayer
  }
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

trait XYLayoutPart extends AbstractGraphicalEditPart{
  def resizeCommand(res:Resizable, r:Rectangle) = new Command() {
      override def execute() = res.pos = (r.x,r.y); res.size=(r.width,r.height)
    }
  def positionCommand(pos:Positional, p : org.eclipse.draw2d.geometry.Point):Command =new Command() {
      override def execute() = pos.pos = (p.x,p.y)
    }
  def createCommand(req:CreateRequest):Command = null
  def resizableChild = false

  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.LAYOUT_ROLE, new XYLayoutEditPolicy(){
        override protected def 
          createChangeConstraintCommand(child: EditPart, constraint : Object) :Command = 
            (child.getModel,constraint) match 
            {case (c:Resizable, rect:Rectangle) => resizeCommand(c,rect)
            case (p:Positional, rect:Rectangle) => positionCommand(p,rect.getTopLeft)}
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
trait SimpleNodePart extends AbstractGraphicalEditPart with NodeEditPart{
  def anchor : ConnectionAnchor
  override def getSourceConnectionAnchor(connection:ConnectionEditPart)= anchor
  override def getSourceConnectionAnchor(connection:Request)           = anchor
  override def getTargetConnectionAnchor(connection:ConnectionEditPart)= anchor
  override def getTargetConnectionAnchor(connection:Request)           = anchor  
  override abstract protected def createEditPolicies{
    installEditPolicy(EditPolicy.GRAPHICAL_NODE_ROLE, new GraphicalNodeEditPolicy(){
      private def  reconnect(req: ReconnectRequest) = null
      protected def getReconnectTargetCommand(req :ReconnectRequest) = reconnect(req)
      protected def getReconnectSourceCommand(req : ReconnectRequest) = reconnect(req)
      protected def getConnectionCreateCommand(req : CreateConnectionRequest) = null
      protected def getConnectionCompleteCommand(req : CreateConnectionRequest) = null
    });
    super.createEditPolicies
  }
}

trait ConnectionPart extends AbstractConnectionEditPart{
  def delete : Command
  override abstract protected def createEditPolicies{
    installEditPolicy(EditPolicy.CONNECTION_BENDPOINTS_ROLE,
        new BendpointEditPolicy(){
      def getCreateBendpointCommand(req : BendpointRequest):Command=null;
      def getDeleteBendpointCommand(req : BendpointRequest):Command=null;
      def getMoveBendpointCommand(req : BendpointRequest):Command=null;
    });
    installEditPolicy(EditPolicy.CONNECTION_ENDPOINTS_ROLE,
        new ConnectionEndpointEditPolicy());
    installEditPolicy(EditPolicy.CONNECTION_ROLE, new ConnectionEditPolicy(){
      def getDeleteCommand(r: GroupRequest) = delete
    });
    super.createEditPolicies
  }
}

trait SnapPart extends AbstractGraphicalEditPart {
   override abstract def activate() = {
    getViewer().setProperty(SnapToGrid.PROPERTY_GRID_ENABLED, true)
    getViewer().setProperty(SnapToGrid.PROPERTY_GRID_VISIBLE, true)
    super.activate();
  }
  override abstract def createEditPolicies = {
    super.createEditPolicies
    installEditPolicy("Snap Feedback", new SnapFeedbackPolicy());
  }
  override def getAdapter(adapter : Class[_]) : Object= {
    if (adapter == classOf[SnapToHelper]) {
      val snapStrategies = new ArrayList[SnapToHelper]()
      val v = getViewer().getProperty(RulerProvider.PROPERTY_RULER_VISIBILITY).asInstanceOf[Boolean]
      if (v)
        snapStrategies.add(new SnapToGuides(this))
      val se = getViewer().getProperty(SnapToGeometry.PROPERTY_SNAP_ENABLED).asInstanceOf[Boolean]
      if (se)
        snapStrategies.add(new SnapToGeometry(this))
      val ge = getViewer().getProperty(SnapToGrid.PROPERTY_GRID_ENABLED).asInstanceOf[Boolean]
      if (ge)
        snapStrategies.add(new SnapToGrid(this));

      if (snapStrategies.size() == 0)
        return null;
      if (snapStrategies.size() == 1)
        return snapStrategies.get(0);

      val ss = new Array[SnapToHelper](snapStrategies.size());
      for (i <- 0 to snapStrategies.size())
        ss(i) = snapStrategies.get(i);
      return new CompoundSnapToHelper(ss);
    } else
      return super.getAdapter(adapter);
  }

}
