package org.zaluum.nide.eclipse
import org.eclipse.core.runtime.Path
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.resource.ImageRegistry
import org.zaluum.nide.Activator

object ZaluumPluginImages {
  val ICONS_PATH = new Path("icons"); //$NON-NLS-1$

  // The plug-in registry
  lazy val imageRegistry = new ImageRegistry();
  lazy val bundle = Activator.getDefault().getBundle();

  //public static final String IMG_NEW_GROOVY_PROJECT = "icons/full/newgroovyprj_wiz.gif";
  val IMG_ZALUUM_FILE = "icons/zaluum_file.gif";
  val IMG_ZALUUM_FILE_NO_BUILD = "icons/zaluum_file_nobuild.gif";
  val IMG_ZALUUM_OVERLAY = "icons/zaluum_project_overlay.png";
  //  public static final ImageDescriptor DESC_NEW_GROOVY_PROJECT = createDescriptor(IMG_NEW_GROOVY_PROJECT);
  val DESC_ZALUUM_FILE = createDescriptor(IMG_ZALUUM_FILE);
  val DESC_ZALUUM_FILE_NO_BUILD = createDescriptor(IMG_ZALUUM_FILE_NO_BUILD);
  val DESC_ZALUUM_OVERLAY = createDescriptor(IMG_ZALUUM_OVERLAY);

  def createDescriptor(path: String) = {
    val url = bundle.getEntry(path)
    val descriptor = if (url == null)
      ImageDescriptor.getMissingImageDescriptor()
    else
      ImageDescriptor.createFromURL(url)
    imageRegistry.put(path, descriptor)
    descriptor
  }

  def get(key: String) = imageRegistry.get(key);

  def getDescriptor(key: String) = imageRegistry.getDescriptor(key);
}