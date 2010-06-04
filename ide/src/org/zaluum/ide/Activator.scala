package org.zaluum.ide

import org.eclipse.ui.plugin.AbstractUIPlugin
import org.eclipse.jface.resource.ImageRegistry
import org.osgi.framework.BundleContext
import org.zaluum.ide.icons.Icons

class Activator extends AbstractUIPlugin {

  val PLUGIN_ID : String = "org.zaluum.ide.editor"
  var plugin : Activator = _
  
  override def start(context : BundleContext) = {
    super.start(context);
    plugin = this;
  }
  
  override def initializeImageRegistry(reg : ImageRegistry) = {
    super.initializeImageRegistry(reg);
    //Utils.loadIcons(reg,Icons.getClass,"zaluum","stickynote", "composed", "const", "value", "instance", "portin", "portout", "wire");
    //Utils.loadImage(reg,"portInD", Icons.getClass);
    //Utils.loadImage(reg,"portInND", Icons.getClass);
    //Utils.loadImage(reg,"portOutND", Icons.getClass);
  }

  def getDefault() : Activator = {
    return plugin;
  }
  
  override def getImageRegistry() : ImageRegistry = {
    return super.getImageRegistry();
  }
  
}