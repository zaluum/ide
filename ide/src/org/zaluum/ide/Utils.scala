package org.zaluum.ide

import org.eclipse.jface.resource.ImageRegistry

object Utils {

  def loadIcons(ir : ImageRegistry, base : Class[_], keys : String*) = {
    keys.foreach(k => { loadImage(ir, k + "_16", base); loadImage(ir, k + "_32", base) })
  }
  
  def loadImage(ir : ImageRegistry, key : String, base : Class[_]) = {
    import org.eclipse.jface.resource.ImageDescriptor
    if (ir.getDescriptor(key) == null) {
      ir.put(key, ImageDescriptor.createFromURL(base.getResource(key + ".png")));
    }
  }
  
}