package org.zaluum.ide
import org.zaluum.runtime.{Command=>C,_}
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.Display
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
import org.eclipse.gef.tools.DirectEditManager
import org.eclipse.gef.requests.DirectEditRequest
import org.eclipse.jface.fieldassist.ContentProposalAdapter
import org.eclipse.jface.fieldassist.SimpleContentProposalProvider
import org.eclipse.jface.fieldassist.TextContentAdapter
import org.eclipse.jface.bindings.keys.KeyStroke
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.swt.widgets.Composite
import Commands._
import org.eclipse.gef.tools.DirectEditManager
import org.eclipse.gef.requests.DirectEditRequest
import org.eclipse.jface.fieldassist.ContentProposalAdapter
import org.eclipse.jface.fieldassist.SimpleContentProposalProvider
import org.eclipse.jface.bindings.keys.KeyStroke
import org.eclipse.jface.fieldassist.TextContentAdapter
import org.eclipse.swt.widgets.Composite
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.jface.viewers.LabelProvider

trait BasePart extends AbstractGraphicalEditPart with Observer{
  type F<:Figure
  type S<: Subject
  def model : S
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
  def contents : Array[String]
  def editCommand(v:String) : Command
  private val directManager = new DirectEditManager(this, null, new TextEditorLocator(editFigure)) {
	  def initCellEditor = {
	      getCellEditor.setValue(editFigure.getText)
	      getCellEditor.getControl.setFont(editFigure.getFont)
	      new ContentProposalAdapter(getCellEditor.getControl, new TextContentAdapter, new EditCPP(contents),
          KeyStroke.getInstance("Ctrl+Space"), null).setLabelProvider(
          new LabelProvider(){
            override def getText(o : Object) = {
              o.asInstanceOf[ContentProposal].getContent
            }
          })
	  }
	  override def createCellEditorOn(composite : Composite) = new TextCellEditor(composite)	  
  }
  override def performRequest(req : Request) = req.getType match {
    case RequestConstants.REQ_DIRECT_EDIT => directManager.show
    case _ => super.performRequest(req)
  }
  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.DIRECT_EDIT_ROLE, new org.eclipse.gef.editpolicies.DirectEditPolicy {
	    def getDirectEditCommand(edit:DirectEditRequest) = editCommand(edit.getCellEditor.getValue.toString.replaceAll("\n", ""))
	    def showCurrentEditValue(req : DirectEditRequest) = {
	      editFigure.setText(req.getCellEditor.getValue.toString)
	      getHostFigure.getUpdateManager.performUpdate
	    }
    })
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

trait Updater {
  self : Observer with AbstractGraphicalEditPart =>
  val display = Display.getCurrent
  private def inSWT(toRun: =>Unit){
    if (Display.getCurrent!=null) 
      toRun
    else
      display.asyncExec(new Runnable {override def run {toRun}})
  }
    
  override def receiveUpdate(s: Subject) = inSWT{
    if (isActive) refresh
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

trait HighlightPart extends AbstractGraphicalEditPart {
  def highlightFigure : Shape
  def highlightPolicy : GraphicalEditPolicy = new HighlightEditPolicy {
  	override def containerFigure = highlightFigure 
  }
  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.SELECTION_FEEDBACK_ROLE, highlightPolicy);
    super.createEditPolicies
  }
}

trait NeighborsHighlightPart extends HighlightPart {
	def highlightNeighbors : List[Shape]
	override def highlightPolicy = new NeighborsHighlightEditPolicy(highlightNeighbors) {
      override def containerFigure = highlightFigure 
  }
}

trait XYLayoutPart extends AbstractGraphicalEditPart{
  def resizeCommand(res:Resizable, r:Rectangle):Command = ResizeCommand(res, (r.x,r.y), (r.width,r.height))
  def positionCommand(pos:Positional, p : geometry.Point):Command =PositionCommand(pos,(p.x,p.y))
  def specialPlaceCommand(p:AnyRef, rect:Rectangle) :Command= null
  def createCommand(newObject : AnyRef, r:Rectangle):Command = null

  override abstract protected def createEditPolicies {
    installEditPolicy(EditPolicy.LAYOUT_ROLE, new XYLayoutEditPolicy(){
        override protected def 
          createChangeConstraintCommand(child: EditPart, constraint : Object) :Command = 
            (child.getModel,constraint) match {
              case (c:Resizable, rect:Rectangle) => resizeCommand(c,rect)
              case (p:Positional, rect:Rectangle) => positionCommand(p,rect.getTopLeft)
              case (p, rect:Rectangle) => specialPlaceCommand(p,rect)
          }
        override protected def getCreateCommand(request : CreateRequest) = 
          createCommand(request.getNewObject,getConstraintFor(request).asInstanceOf[Rectangle])
        override protected def createChildEditPolicy(child : EditPart) = child.getModel match{
          case r:Resizable => super.createChildEditPolicy(child)
          case _ =>new NonResizableEditPolicy()
        }
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

case class Start[T](val p:T) extends Command

trait SimpleNodePart extends BasePart with NodeEditPart{
  def anchor : ConnectionAnchor
  override  def getSourceConnectionAnchor(connection:ConnectionEditPart)= anchor
  override  def getSourceConnectionAnchor(connection:Request)           = anchor
  override  def getTargetConnectionAnchor(connection:ConnectionEditPart)= anchor
  override  def getTargetConnectionAnchor(connection:Request)           = anchor  
  protected def connect(source:S) : Command = null
  protected def reconnect(req: ReconnectRequest):Command = null  
  override abstract protected def createEditPolicies{
    installEditPolicy(EditPolicy.GRAPHICAL_NODE_ROLE, new GraphicalNodeEditPolicy(){
      protected def getReconnectTargetCommand(req :ReconnectRequest) = reconnect(req)
      protected def getReconnectSourceCommand(req : ReconnectRequest) = reconnect(req)
      protected def getConnectionCreateCommand(req : CreateConnectionRequest) = {
        val c = Start(model)
        req.setStartCommand(c)
        c
      }
      protected def getConnectionCompleteCommand(req : CreateConnectionRequest) = req.getStartCommand match{
        case Start(source: AnyRef) if (source!=model) => connect(source.asInstanceOf[S])
        case p => null 
      }
    });
    super.createEditPolicies
  }
}
trait ConnectionPart extends AbstractConnectionEditPart{
  def delete : Command = null
  def createBendpoint(p:geometry.Point, i:Int):Command = null
  def deleteBendpoint(i:Int):Command = null
  override abstract protected def createEditPolicies{
    installEditPolicy(EditPolicy.CONNECTION_BENDPOINTS_ROLE,
        new BendpointEditPolicy(){
         private def toBendpoint(request : BendpointRequest)={
            val p = request.getLocation();
            val conn = getConnection();
            conn.translateToRelative(p);
            
            val ref1 = getConnection().getSourceAnchor().getReferencePoint();
            val ref2 = getConnection().getTargetAnchor().getReferencePoint();
            
            conn.translateToRelative(ref1);
            conn.translateToRelative(ref2);
            
            val p1 = p.getDifference(ref1)
            val p2 = p.getDifference(ref2)
            ((p1.width,p1.height),(p2.width,p2.height))
         }
      def getCreateBendpointCommand(req : BendpointRequest):Command=createBendpoint(req.getLocation,req.getIndex)
      def getDeleteBendpointCommand(req : BendpointRequest):Command=deleteBendpoint(req.getIndex);
      def getMoveBendpointCommand(req : BendpointRequest):Command= { 
        val c= new CompoundCommand()
        c.add(deleteBendpoint(req.getIndex))
        c.add(createBendpoint(req.getLocation,req.getIndex))
        c
      }
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
  override def getAdapter(adapter : Class[_]) = {
    if (adapter == classOf[SnapToHelper]) {
      def prop(s:String) = getViewer().getProperty(s).asInstanceOf[Boolean]
      val snapStrategies = Buffer[SnapToHelper]()
      if (prop(RulerProvider.PROPERTY_RULER_VISIBILITY))
        snapStrategies.add(new SnapToGuides(this))
      if (prop(SnapToGeometry.PROPERTY_SNAP_ENABLED))
        snapStrategies.add(new SnapToGeometry(this))
      if (prop(SnapToGrid.PROPERTY_GRID_ENABLED))
        snapStrategies.add(new SnapToGrid(this))
      if (snapStrategies.size == 0) 
        null
      else if (snapStrategies.size == 1)
        snapStrategies.get(0)
      else  
        new CompoundSnapToHelper(Array.tabulate(snapStrategies.size)(snapStrategies.get(_)))
    } else
      super.getAdapter(adapter);
  }

}

trait RefPropertySource extends BasePart with IPropertySource{
  def getEditableValue = model  
  def properties : List[Property[_]]
  def toDescriptor : PartialFunction[Property[_],IPropertyDescriptor]= {
      case p: Property[_] => new PropertyDescriptor(p, p.desc)
  }
  lazy val getPropertyDescriptors : Array[IPropertyDescriptor] = (properties map toDescriptor).toArray  
  def isPropertySet(id : Object) = false 
  def resetPropertyValue(id : Object) { }
  override def getPropertyValue(id : Object) : Object =  id.asInstanceOf[Property[AnyRef]].get()
  override def setPropertyValue(id:Object, value:Object)  {}
  abstract override def getAdapter(key: Class[_]) = {
    if (key == classOf[IPropertySource]) 
      this
    else super.getAdapter(key)
  }
}
trait RefPropertySourceWrite extends RefPropertySource{
  override def toDescriptor : PartialFunction[Property[_],IPropertyDescriptor]= {
      case str: StringProperty => new TextPropertyDescriptor(str, str.desc)
      case b : BooleanProperty => new CheckboxPropertyDescriptor(b, b.desc)
  }
  override def setPropertyValue(id:Object, value:Object)  {
    val o = id.asInstanceOf[Property[AnyRef]].set
    
    model.notifyObservers
  }
}
