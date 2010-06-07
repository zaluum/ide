package org.zaluum.ide
import org.zaluum.runtime._
import org.eclipse.swt.SWT
import org.eclipse.draw2d._
import org.eclipse.core.runtime._
import org.eclipse.gef._
import org.eclipse.gef.palette.PaletteRoot
import org.eclipse.gef.ui.parts._
import org.eclipse.gef.editparts._
import org.eclipse.gef.ui.actions._
import org.eclipse.ui._
import org.eclipse.ui.actions._
import org.eclipse.swt.graphics._
import org.eclipse.draw2d.geometry.Rectangle
import org.zaluum.example.Example
import org.eclipse.jface.action._;
import scala.collection.JavaConversions._
import scala.collection.mutable._
import java.util.ArrayList

abstract class BaseEditor extends GraphicalEditorWithFlyoutPalette {
  var gridColor : Color = null
  setEditDomain(new DefaultEditDomain(this));
  def model:AnyRef
  
  def editDomain : EditDomain = { getEditDomain }
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
    val snapAction = new ToggleSnapToGeometryAction(getGraphicalViewer())
    getActionRegistry().registerAction(snapAction)

    val showGrid = new ToggleGridAction(getGraphicalViewer())
    getActionRegistry().registerAction(showGrid)
    
    val connectionLayer = rootEditPart
        .getLayer(LayerConstants.CONNECTION_LAYER).asInstanceOf[ConnectionLayer]
    connectionLayer.setConnectionRouter(new BendpointConnectionRouter())
    getGraphicalViewer().setContents(model)
  }
  protected def addAction(a:IAction) {
    val registry = getActionRegistry
    registry.registerAction(a)
    val l = getSelectionActions.asInstanceOf[java.util.List[Object]]
                                             l.add(a.getId)
  }
  override def createActions {
    super.createActions()
    val w = this.asInstanceOf[IWorkbenchPart]
//    add(new CopyAction(w))
   
    addAction(new AlignmentAction(w,PositionConstants.LEFT))
    addAction(new AlignmentAction(w,PositionConstants.RIGHT))
    addAction(new AlignmentAction(w,PositionConstants.TOP))
    addAction(new AlignmentAction(w,PositionConstants.BOTTOM))
    addAction(new AlignmentAction(w,PositionConstants.CENTER))
    addAction(new AlignmentAction(w,PositionConstants.MIDDLE))
    addAction(new MatchHeightAction(w))
    addAction(new MatchWidthAction(w))
  }
  def factory : EditPartFactory 
  override def configureGraphicalViewer() = {
    super.configureGraphicalViewer();
    val viewer = getGraphicalViewer();
    viewer.setEditPartFactory(factory);
    viewer.setRootEditPart(new ScalableFreeformRootEditPart());
    viewer.setKeyHandler(new GraphicalViewerKeyHandler(viewer));
    val gridColorDesc = PlatformUI.getWorkbench().getThemeManager()
      .getCurrentTheme().getColorRegistry()
      .getColorDescriptor("org.zaluum.ide.gridColor");
    gridColor = gridColorDesc.createColor(viewer.getControl().getDisplay());
    
    // configure the context menu provider
    val cmProvider = new BaseContextMenuProvider(viewer, getActionRegistry())
    viewer.setContextMenu(cmProvider);
  }
}


class BaseContextMenuProvider(viewer:EditPartViewer, val registry:ActionRegistry) 
        extends ContextMenuProvider(viewer) {

  override def buildContextMenu(menu : IMenuManager) {
    import GEFActionConstants._
    import org.eclipse.ui.actions.ActionFactory
    // Add standard action groups to the menu
    addStandardActionGroups(menu);
    // Add actions to the menu
    def addm(s:String,a:String) = menu.appendToGroup(s,action(a))
    addm(GROUP_UNDO,UpAction.ID);
    addm(GROUP_UNDO,ActionFactory.UNDO.getId)
    addm(GROUP_UNDO,ActionFactory.REDO.getId)
    addm(GROUP_EDIT,ActionFactory.DELETE.getId)

    /*menu.appendToGroup(GEFActionConstants.GROUP_EDIT, 
        action(InstanceAction.ID));
    menu.appendToGroup(GEFActionConstants.GROUP_EDIT, 
        action(GoToDeclarationAction.ID));*/

   /* addm(GROUP_COPY,ActionFactory.CUT.getId);
    addm(GROUP_COPY,ActionFactory.COPY.getId);
    addm(GROUP_COPY,ActionFactory.PASTE.getId);
*/
    val submenu = new MenuManager("&Align");
    def add(s:String){
      val a = action(s)
      if (a.isEnabled())
        submenu.add(a)  
    }
    add(GEFActionConstants.ALIGN_LEFT)
    add(GEFActionConstants.ALIGN_CENTER);
    add(GEFActionConstants.ALIGN_RIGHT);
    submenu.add(new Separator());
    
    add(GEFActionConstants.ALIGN_TOP);
    add(GEFActionConstants.ALIGN_MIDDLE);
    add(GEFActionConstants.ALIGN_BOTTOM);
    if (!submenu.isEmpty())
      menu.appendToGroup(GEFActionConstants.GROUP_REST, submenu);
    
  }

  private def action(actionId:String) = registry.getAction(actionId);
  

}

class BaseActionBarContributor extends ActionBarContributor
{

 override protected def buildActions {
  addRetargetAction(new UndoRetargetAction());
  addRetargetAction(new RedoRetargetAction());
  
  val iww = getPage().getWorkbenchWindow();
  addRetargetAction(new DeleteRetargetAction());
  /*addRetargetAction(ActionFactory.CUT.create(iww));
  addRetargetAction((RetargetAction)ActionFactory.COPY.create(iww));
  addRetargetAction((RetargetAction)ActionFactory.PASTE.create(iww));
  */
  addRetargetAction(new AlignmentRetargetAction(PositionConstants.LEFT));
  addRetargetAction(new AlignmentRetargetAction(PositionConstants.CENTER));
  addRetargetAction(new AlignmentRetargetAction(PositionConstants.RIGHT));
  addRetargetAction(new AlignmentRetargetAction(PositionConstants.TOP));
  addRetargetAction(new AlignmentRetargetAction(PositionConstants.MIDDLE));
  addRetargetAction(new AlignmentRetargetAction(PositionConstants.BOTTOM));
  
  addRetargetAction(new ZoomInRetargetAction());
  addRetargetAction(new ZoomOutRetargetAction());
  
  addRetargetAction(new MatchWidthRetargetAction());
  addRetargetAction(new MatchHeightRetargetAction());
  
  addRetargetAction(new RetargetAction(
      GEFActionConstants.TOGGLE_SNAP_TO_GEOMETRY, 
      "Snap to Geometry", IAction.AS_CHECK_BOX));

  addRetargetAction(new RetargetAction(GEFActionConstants.TOGGLE_GRID_VISIBILITY, 
      "Snap to Grid", IAction.AS_CHECK_BOX));
  
  addRetargetAction(new RetargetAction(UpAction.ID, "Up"));
}

override protected def declareGlobalActionKeys() {
  addGlobalActionKey(ActionFactory.PRINT.getId());
  addGlobalActionKey(ActionFactory.SELECT_ALL.getId());
  addGlobalActionKey(ActionFactory.PASTE.getId());
  addGlobalActionKey(ActionFactory.DELETE.getId());
}

override def contributeToToolBar(tbm : IToolBarManager) {
  tbm.add(getAction(ActionFactory.UNDO.getId()));
  tbm.add(getAction(ActionFactory.REDO.getId()));
  tbm.add(new Separator());
  tbm.add(getAction("org.zaluum.ide.editor.up"));
  tbm.add(new Separator());
  tbm.add(getAction(GEFActionConstants.ALIGN_LEFT));
  tbm.add(getAction(GEFActionConstants.ALIGN_CENTER));
  tbm.add(getAction(GEFActionConstants.ALIGN_RIGHT));
  tbm.add(new Separator());
  tbm.add(getAction(GEFActionConstants.ALIGN_TOP));
  tbm.add(getAction(GEFActionConstants.ALIGN_MIDDLE));
  tbm.add(getAction(GEFActionConstants.ALIGN_BOTTOM));
  
  tbm.add(new Separator()); 
  tbm.add(getAction(GEFActionConstants.MATCH_WIDTH));
  tbm.add(getAction(GEFActionConstants.MATCH_HEIGHT));
  
  tbm.add(new Separator()); 
  val zoomStrings = Array(ZoomManager.FIT_ALL, 
                      ZoomManager.FIT_HEIGHT, 
                      ZoomManager.FIT_WIDTH );
  tbm.add(new ZoomComboContributionItem(getPage(), zoomStrings));
}

override def contributeToMenu(menubar : IMenuManager) {
  super.contributeToMenu(menubar);
  val viewMenu = new MenuManager("&View");
  viewMenu.add(getAction(UpAction.ID));
  viewMenu.add(getAction(GEFActionConstants.ZOOM_IN));
  viewMenu.add(getAction(GEFActionConstants.ZOOM_OUT));
  viewMenu.add(new Separator());
  viewMenu.add(getAction(GEFActionConstants.TOGGLE_GRID_VISIBILITY));
  viewMenu.add(getAction(GEFActionConstants.TOGGLE_SNAP_TO_GEOMETRY));
  viewMenu.add(new Separator());
  viewMenu.add(getAction(GEFActionConstants.MATCH_WIDTH));
  viewMenu.add(getAction(GEFActionConstants.MATCH_HEIGHT));
  menubar.insertAfter(IWorkbenchActionConstants.M_EDIT, viewMenu);
}

}
