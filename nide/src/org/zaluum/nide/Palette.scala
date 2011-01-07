package org.zaluum.nide

import org.eclipse.gef.palette.ConnectionCreationToolEntry
import org.eclipse.gef.palette.CreationToolEntry
import org.eclipse.gef.palette.PaletteContainer
import org.eclipse.gef.palette.PaletteRoot
import org.eclipse.gef.palette.PaletteToolbar
import org.eclipse.gef.palette.PanningSelectionToolEntry
import org.eclipse.gef.palette.ToolEntry
import org.eclipse.gef.requests.CreationFactory
import org.eclipse.jface.resource.{ImageDescriptor,ImageRegistry}


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
			/*toolbar.add(new ConnectionCreationToolEntry("Zaluum Tool", "Multi-tool for editing", new CreationFactory() {
        def getObjectType() = classOf[VWire]
        def getNewObject() = classOf[VWire]
      }, ir.getDescriptor("wire_16"), ir.getDescriptor("wire_32")))*/
			palette.setDefaultEntry(tool)
			toolbar
		});
    val boxToolbar = new PaletteToolbar("Box Tools")
    //boxToolbar.add(newTool("Composed Box","A Box Composed of other boxes", "composed", classOf[ComposedPBox],classOf[Icons]))
    //boxToolbar.add(newTool("Port","Port to input and output data", "portin", classOf[PPort],classOf[Icons]))
    palette.add(boxToolbar)
		palette
	}
}

class ZaluumToolEntry(label : String, shortDesc : String, factory : CreationFactory, iconSmall : ImageDescriptor, iconLarge : ImageDescriptor) 
  extends ToolEntry(label, shortDesc, iconSmall, iconLarge, classOf[ZaluumTool]);


class ZaluumTool extends TargetingTool {

  val connection = new ConnectionCreationTool
  val selection = new PanningSelectionTool
  var tool : TargetingTool = selection
  
  override def getCommandName() = {
    null
  }
}
