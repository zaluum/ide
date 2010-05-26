package org.zaluum.ide
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
import org.eclipse.swt.graphics._
import scala.collection.JavaConversions._
import scala.collection.mutable._
import java.util.ArrayList
import org.eclipse.draw2d.geometry.Rectangle
case class LightBox(name:String)
class Model {
  val boxes = List(LightBox("box1"),LightBox("box2"))
}

class ModelEditPart(val model : Model) extends AbstractGraphicalEditPart{
  var currentBox : Int = 0
  setModel(model)

  override def activate() = {
    getViewer().setProperty(SnapToGrid.PROPERTY_GRID_ENABLED, true)
    getViewer().setProperty(SnapToGrid.PROPERTY_GRID_VISIBLE, true)
    super.activate();
  }

  override def deactivate() = super.deactivate()

  override def createFigure : IFigure = {
    val freeformLayer = new FreeformLayer()
    freeformLayer.setLayoutManager(new FreeformLayout())
    freeformLayer
  }

  override def createEditPolicies() = {
    installLayoutPolicies();
    installEditPolicy("Snap Feedback", new SnapFeedbackPolicy());
  }

  override def getModelChildren():java.util.List[AnyRef] = model.boxes 

  def up() = {}

  private def installLayoutPolicies() = {
    installEditPolicy(EditPolicy.LAYOUT_ROLE, new XYLayoutEditPolicy(){
      override def createChangeConstraintCommand(child: EditPart, constraint : AnyRef):Command = UnexecutableCommand.INSTANCE
      override def getCreateCommand(req:CreateRequest):Command = UnexecutableCommand.INSTANCE
    })
  }
  def getCurrentBox() = currentBox

  override def getAdapter(adapter : Class[_]):AnyRef = {
    if (adapter == classOf[SnapToHelper]) { 
      val snapStrategies = new ArrayList[AnyRef]()
      var snap = getViewer().getProperty(
          RulerProvider.PROPERTY_RULER_VISIBILITY).asInstanceOf[Boolean];
      if (snap)
        snapStrategies.add(new SnapToGuides(this));
      val geom = getViewer().getProperty(SnapToGeometry.PROPERTY_SNAP_ENABLED).asInstanceOf[Boolean];
      if (geom)
        snapStrategies.add(new SnapToGeometry(this));
      val grid = getViewer().getProperty(SnapToGrid.PROPERTY_GRID_ENABLED).asInstanceOf[Boolean]
      if (grid)
        snapStrategies.add(new SnapToGrid(this));

      if (snapStrategies.size() == 0)
        return null
      if (snapStrategies.size() == 1)
        return snapStrategies.get(0);

      val ss = new Array[SnapToHelper](snapStrategies.size());
      for (i <- 0 until snapStrategies.size()) {
        ss(i) = snapStrategies.get(i).asInstanceOf[SnapToHelper];
      }
      new CompoundSnapToHelper(ss);
    }else {
      super.getAdapter(adapter);
    }
  }
}
class BoxEditPart(val box : LightBox) extends AbstractGraphicalEditPart{
  override def createFigure():IFigure = {
    new RectangleFigure();
  }
  private def parentGEP : GraphicalEditPart = getParent.asInstanceOf[GraphicalEditPart]
  override def refreshVisuals() = {
    parentGEP.setLayoutConstraint(this, getFigure, new Rectangle(50,50,100,100))
  }
  override def createEditPolicies : Unit ={
  }
}

class ZaluumFactory extends EditPartFactory {
  def createEditPart(context: EditPart, model: Object): EditPart = model match { 
    case model : Model => new ModelEditPart(model)
    case box : LightBox => new BoxEditPart(box)
  }
}
class Editor extends GraphicalEditorWithFlyoutPalette {
  val model  = new Model()
  var gridColor : Color = null
  setEditDomain(new DefaultEditDomain(this));
  override def doSave(monitor: IProgressMonitor) = {
    
  }
  override def getPaletteRoot : PaletteRoot = {
    null
  }
  override def initializeGraphicalViewer():Unit  = {
    super.initializeGraphicalViewer()
    if (!getEditorInput().exists()) return
    val viewer = getGraphicalViewer();
    val rootEditPart = viewer.getRootEditPart().asInstanceOf[ScalableFreeformRootEditPart];
    rootEditPart.getLayer(LayerConstants.GRID_LAYER).setForegroundColor(gridColor);
    
    var zoomLevels : java.util.List[String] = ArrayBuffer[String](ZoomManager.FIT_ALL, ZoomManager.FIT_WIDTH, ZoomManager.FIT_HEIGHT)
    rootEditPart.getZoomManager().setZoomLevelContributions(zoomLevels);

    val zoomIn = new ZoomInAction(rootEditPart.getZoomManager());
    val zoomOut = new ZoomOutAction(rootEditPart.getZoomManager());
    getActionRegistry().registerAction(zoomIn);
    getActionRegistry().registerAction(zoomOut);
    getGraphicalViewer().setProperty(MouseWheelHandler.KeyGenerator.getKey(SWT.MOD1), 
        MouseWheelZoomHandler.SINGLETON);
    
    // Actions
    val snapAction = new ToggleSnapToGeometryAction(getGraphicalViewer());
    getActionRegistry().registerAction(snapAction);

    val showGrid = new ToggleGridAction(getGraphicalViewer());
    getActionRegistry().registerAction(showGrid);
    
    val connectionLayer = rootEditPart
        .getLayer(LayerConstants.CONNECTION_LAYER).asInstanceOf[ConnectionLayer];
    connectionLayer.setConnectionRouter(new BendpointConnectionRouter());
    getGraphicalViewer().setContents(model);
  }

  override def configureGraphicalViewer() = {
    super.configureGraphicalViewer();
    val viewer = getGraphicalViewer();
    viewer.setEditPartFactory(new ZaluumFactory());
    viewer.setRootEditPart(new ScalableFreeformRootEditPart());
    viewer.setKeyHandler(new GraphicalViewerKeyHandler(viewer));
    val gridColorDesc = PlatformUI.getWorkbench().getThemeManager()
      .getCurrentTheme().getColorRegistry()
      .getColorDescriptor("org.zaluum.ide.gridColor");
    gridColor = gridColorDesc.createColor(viewer.getControl().getDisplay());
    
    // configure the context menu provider
    /*ContextMenuProvider cmProvider = new ModelEditorContextMenuProvider(
        viewer, getActionRegistry());
    viewer.setContextMenu(cmProvider);*/
  }

}