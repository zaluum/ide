package org.zaluum.nide.eclipse;

import java.net.URL;

import org.zaluum.nide.Activator;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.swt.graphics.Image;
import org.osgi.framework.Bundle;

public class ZaluumPluginImages {

  public static final IPath ICONS_PATH= new Path("icons"); //$NON-NLS-1$

  // The plug-in registry
  private static final ImageRegistry imageRegistry= new ImageRegistry();
  private static final Bundle bundle = Activator.getDefault().getBundle();

  //public static final String IMG_NEW_GROOVY_PROJECT = "icons/full/newgroovyprj_wiz.gif";
  public static final String IMG_ZALUUM_FILE = "icons/zaluum_file.gif";
  public static final String IMG_ZALUUM_FILE_NO_BUILD = "icons/zaluum_file_nobuild.gif";
  public static final String IMG_ZALUUM_OVERLAY = "icons/zaluum_project_overlay.png";
//  public static final ImageDescriptor DESC_NEW_GROOVY_PROJECT = createDescriptor(IMG_NEW_GROOVY_PROJECT);
  public static final ImageDescriptor DESC_ZALUUM_FILE = createDescriptor(IMG_ZALUUM_FILE);
  public static final ImageDescriptor DESC_ZALUUM_FILE_NO_BUILD = createDescriptor(IMG_ZALUUM_FILE_NO_BUILD);
  public static final ImageDescriptor DESC_ZALUUM_OVERLAY = createDescriptor(IMG_ZALUUM_OVERLAY);

  public static ImageDescriptor createDescriptor(String path) {
        URL url = bundle.getEntry(path);
        ImageDescriptor descriptor = url == null ?
                ImageDescriptor.getMissingImageDescriptor() :
                    ImageDescriptor.createFromURL(url);
        imageRegistry.put(path, descriptor);
        return descriptor;
    }

  public static Image get(String key) {
    return imageRegistry.get(key);
  }

  public static ImageDescriptor getDescriptor(String key) {
    return imageRegistry.getDescriptor(key);
  }
}
