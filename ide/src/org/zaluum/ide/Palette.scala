package org.zaluum.ide

import org.eclipse.gef.palette.ConnectionCreationToolEntry
import org.eclipse.gef.palette.CreationToolEntry
import org.eclipse.gef.palette.PaletteContainer
import org.eclipse.gef.palette.PaletteRoot
import org.eclipse.gef.palette.PaletteToolbar
import org.eclipse.gef.palette.PanningSelectionToolEntry
import org.eclipse.gef.palette.ToolEntry
import org.eclipse.gef.requests.CreationFactory
import org.eclipse.gef.tools.AbstractTool
import org.zaluum.ide.icons.Icons
import org.eclipse.jface.resource.ImageRegistry
import org.zaluum.runtime._
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
			//toolbar.add(newTool("Sticky Note", "Note to write things...", "stickynote", StickyNote.class, Icons.class))
			palette.setDefaultEntry(tool)
			toolbar
		});
    val boxToolbar = new PaletteToolbar("Box Tools")
     boxToolbar.add(newTool("Composed Box","A Box Composed of other boxes", "composed", classOf[ComposedVBox],classOf[Icons]));
     boxToolbar.add(newTool("Input port","Input port", "portin", classOf[VPort],classOf[Icons]));
     boxToolbar.add(newTool("Output port","Ouput port", "portout", classOf[VPort],classOf[Icons]));
     //boxToolbar.add(newTool("Composed Box","A Box Composed of other boxes", "composed", classOf[ComposedVBox],classOf[Icons]));

      //boxToolbar.add(newTool("Instance Box","An instance of another user model", "instance", InstanceVBox.getClass, Icons.getClass));
      //boxToolbar.add(newTool("Constant Box", "A constant output box", "const", ConstVBox.getClass, Icons.getClass));
      //boxToolbar.add(newTool("Value Box", "Reads or writes values from external device", "value", ValueVBox.getClass, Icons.getClass));
      boxToolbar.add(new ConnectionCreationToolEntry("Wire",
        "Creates a new Wire", new CreationFactory() {
          def getObjectType() = classOf[VWire]
          def getNewObject() = classOf[VWire]
        }, ir.getDescriptor("wire_16.png"),
        ir.getDescriptor("wire_32.png")));
   
    palette.add(boxToolbar)
		palette
	}
}