package org.zaluum.ide

import org.eclipse.gef.palette.ConnectionCreationToolEntry
import org.eclipse.gef.palette.CreationToolEntry
import org.eclipse.gef.palette.PaletteContainer
import org.eclipse.gef.palette.PaletteRoot
import org.eclipse.gef.palette.PaletteToolbar
import org.eclipse.gef.palette.PanningSelectionToolEntry
import org.eclipse.gef.palette.ToolEntry
import org.eclipse.gef.requests.CreationFactory
import org.zaluum.ide.icons.Icons
import org.eclipse.jface.resource.{ImageDescriptor,ImageRegistry}
import org.zaluum.runtime._
import org.eclipse.gef.tools._
import org.eclipse.gef.Tool
import org.eclipse.gef.EditDomain
import org.eclipse.gef.EditPartViewer
import org.eclipse.swt.dnd.DragSourceEvent
import org.eclipse.swt.events.FocusEvent
import org.eclipse.swt.events.KeyEvent
import org.eclipse.swt.events.MouseEvent
import org.eclipse.swt.events.MouseTrackListener
import org.eclipse.swt.events.TraverseEvent
import org.eclipse.swt.widgets.Event
import java.util.Map
import org.eclipse.swt.SWT

import org.eclipse.swt.graphics.Cursor;

import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPartListener;
import org.eclipse.gef.EditPartViewer;
import org.eclipse.gef.Request;
import org.eclipse.gef.RequestConstants;
import org.eclipse.gef.SharedCursors;
import org.eclipse.gef.commands.Command;
import org.eclipse.gef.requests.CreateConnectionRequest;
import org.eclipse.gef.requests.CreateRequest;
import org.eclipse.gef.requests.CreationFactory;
import org.eclipse.gef.requests.SelectionRequest;

object Palette {
  def apply() = {
		val palette = new PaletteRoot()
		val ir : ImageRegistry = Activator.getDefault().getImageRegistry();		
		def newTool(name:String, desc:String, resourceKey:String, modelClass:Class[_], baseResources:Class[_]):CreationToolEntry = {
		  new CreationToolEntry(name, desc, new CreationFactory() {
		    def getObjectType():Class[_] = { modelClass }
		    def getNewObject():Class[_] = { modelClass }
		  }, ir.getDescriptor(resourceKey + "_16"), ir.getDescriptor(resourceKey + "_32"));
		}
		palette.add({
			val toolbar = new PaletteToolbar("Basic Tools")
			val tool = new PanningSelectionToolEntry()
			toolbar.add(tool)
			toolbar.add(new ConnectionCreationToolEntry("Zaluum Tool", "Multi-tool for editing", new CreationFactory() {
        def getObjectType() = classOf[VWire]
        def getNewObject() = classOf[VWire]
      }, ir.getDescriptor("wire_16"), ir.getDescriptor("wire_32")))
			palette.setDefaultEntry(tool)
			toolbar
		});
    val boxToolbar = new PaletteToolbar("Box Tools")
    boxToolbar.add(newTool("Composed Box","A Box Composed of other boxes", "composed", classOf[ComposedPBox],classOf[Icons]))
    boxToolbar.add(newTool("Port","Port", "portin", classOf[PPort],classOf[Icons]))
    palette.add(boxToolbar)
		palette
	}
}

class ZaluumToolEntry(label : String, shortDesc : String, factory : CreationFactory, iconSmall : ImageDescriptor, iconLarge : ImageDescriptor) 
  extends ToolEntry(label, shortDesc, iconSmall, iconLarge, classOf[ZaluumTool]);

class ZaluumTool extends SelectionTool {
  
  setDefaultCursor(SharedCursors.CURSOR_PLUG)
  setDisabledCursor(SharedCursors.CURSOR_PLUG_NOT)
  var TOOL_CONNECTION = false
  val STATE_INITIAL = 1
  val STATE_INVALID = 8
  val STATE_TERMINAL = 1 << 30
  val STATE_ACCESSIBLE_DRAG_IN_PROGRESS = 32
  val STATE_CONNECTION_STARTED = 32 << 1
  val MAX_STATE = STATE_CONNECTION_STARTED
  val FLAG_SOURCE_FEEDBACK = 8 << 3
  val MAX_FLAG = FLAG_SOURCE_FEEDBACK
  var connectionSource : EditPart = _
  var factory : CreationFactory = _
  var viewer : EditPartViewer = _

  var deactivationListener : EditPartListener.Stub = new EditPartListener.Stub() {
    override def partDeactivated(editpart : EditPart) = {
      handleSourceDeactivated()
    }
  };

  def ZaluumTool(factory : CreationFactory) {
    this.factory = factory
  }

  def changeTool(state : Boolean) = {
    TOOL_CONNECTION = state
    refreshCursor
    if(state) {
      setTargetRequest(new CreateConnectionRequest())
    }else{
      setTargetRequest(new SelectionRequest)
    }
  }
  
  override def calculateCursor() : Cursor = {
    if(!TOOL_CONNECTION) {
      return SharedCursors.CURSOR_TREE_MOVE
    }else{
      if (isInState(STATE_INITIAL)) {
        if (getCurrentCommand() != null)
          return getDefaultCursor();
      }
      return super.calculateCursor
    }
  }
  
  override def createTargetRequest() : Request = {
      var req : CreateRequest = new CreateConnectionRequest();
      req.setFactory(factory);
      return req;
  }
  
  override def deactivate() = {
      eraseSourceFeedback()
      setConnectionSource(null)
      super.deactivate()
      setState(STATE_TERMINAL)
      viewer = null
  }
  
  def eraseSourceFeedback() = {
    if (isShowingSourceFeedback()) {
      setFlag(FLAG_SOURCE_FEEDBACK, false);
      if (connectionSource != null) {
        connectionSource.eraseSourceFeedback(getSourceRequest());
      }
    }
  }
  
  override def getCommandName() : String = {
      if (isInState(STATE_CONNECTION_STARTED | STATE_ACCESSIBLE_DRAG_IN_PROGRESS))
        return RequestConstants.REQ_CONNECTION_END
      else
        return RequestConstants.REQ_CONNECTION_START
  }
  
  override def getDebugName() : String = {
      return "Connection Creation Tool"
  }
  
  override def getDebugNameForState(s : Int) : String = {
      if (s == STATE_CONNECTION_STARTED || s == STATE_ACCESSIBLE_DRAG_IN_PROGRESS)
        return "Connection Started";
      return super.getDebugNameForState(s);
  }

  def getSourceRequest() = { getTargetRequest() }
 
  override def handleCommandStackChanged() : Boolean = {
      if (!isInState(STATE_INITIAL)) {
        if (getCurrentInput().isMouseButtonDown(1))
          setState(STATE_INVALID);
        else
          setState(STATE_INITIAL);  
        handleInvalidInput();
        return true;
      }
      return false;
  }
  
  def handleCreateConnection() : Boolean = {
    eraseSourceFeedback();
    val endCommand : Command = getCommand();
    setCurrentCommand(endCommand);
    executeCurrentCommand();
    return true;
  }
  
  override def handleDrag() : Boolean = {
      if (isInState(STATE_CONNECTION_STARTED))
        return handleMove();
      return false;
  }
  
  override def handleDragInProgress() : Boolean = {
      if (isInState(STATE_ACCESSIBLE_DRAG_IN_PROGRESS))
        return handleMove();
      return false;
  }
  
  override def handleFocusLost() : Boolean = {
      if (isInState(STATE_CONNECTION_STARTED)) {
        eraseSourceFeedback();
        eraseTargetFeedback();
        setState(STATE_INVALID);
        handleFinished();
      }
      return super.handleFocusLost();
  }
  
  override def handleHover() : Boolean = {
      if (isInState(STATE_CONNECTION_STARTED))
        updateAutoexposeHelper();
      return true;
  }
  
  override def handleInvalidInput() : Boolean = {
      eraseSourceFeedback();
      setConnectionSource(null);
      return super.handleInvalidInput();
  }
  
  
  override def handleButtonDown(button : Int) : Boolean = {
      if (button == 1 && stateTransition(STATE_CONNECTION_STARTED, STATE_TERMINAL))
        return handleCreateConnection()
      if (isInState(STATE_INITIAL) && button == 1) {
        updateTargetRequest();
        updateTargetUnderMouse();
        setConnectionSource(getTargetEditPart());
        var command : Command = getCommand()
        getTargetRequest.asInstanceOf[CreateConnectionRequest].setSourceEditPart(getTargetEditPart)
        if (command != null) {
          setState(STATE_CONNECTION_STARTED);
          setCurrentCommand(command);
          viewer = getCurrentViewer();
        }
      }    
      if (isInState(STATE_INITIAL) && button != 1) {
        setState(STATE_INVALID);
        handleInvalidInput();
      }
      if (isInState(STATE_CONNECTION_STARTED))
        handleDrag();
      return true;
  }
  
  override def handleButtonUp(button : Int) : Boolean = {
      if (isInState(STATE_TERMINAL | STATE_INVALID))
        handleFinished();
      return true;
  }
  
  override def handleMove() : Boolean = {
    if(!getTargetEditPart.isInstanceOf[PortEditPart] && !isInState(STATE_CONNECTION_STARTED)) {
      super.handleMove
    }else{
      if (isInState(STATE_CONNECTION_STARTED) && viewer != getCurrentViewer())
        return false;
      if (isInState(STATE_CONNECTION_STARTED | STATE_INITIAL 
          | STATE_ACCESSIBLE_DRAG_IN_PROGRESS)) {
        updateTargetRequest();
        println(updateTargetUnderMouse())
        showSourceFeedback();
        showTargetFeedback();
        println("Current command : " + getCommand)
        setCurrentCommand(getCommand());
      }
      return true;
    }
  }
  
  def handleSourceDeactivated() = {
    setState(STATE_INVALID);
    handleInvalidInput();
    handleFinished();
  }
  
  def isShowingSourceFeedback() = { getFlag(FLAG_SOURCE_FEEDBACK) }
  
  def setConnectionSource(source : EditPart) = {
    if (connectionSource != null)
      connectionSource.removeEditPartListener(deactivationListener);
    connectionSource = source;
    if (connectionSource != null)
      connectionSource.addEditPartListener(deactivationListener);
  }
  
  def showSourceFeedback() = {
    if (connectionSource != null) {
      connectionSource.showSourceFeedback(getSourceRequest());
    }
    setFlag(FLAG_SOURCE_FEEDBACK, true);
  }
  
  override def updateTargetRequest() = {
      var request : CreateConnectionRequest = getTargetRequest.asInstanceOf[CreateConnectionRequest]
      request.setType(getCommandName());
      request.setLocation(getLocation());
  }
}
