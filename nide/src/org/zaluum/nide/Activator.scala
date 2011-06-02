package org.zaluum.nide
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.ui.plugin.AbstractUIPlugin
import org.osgi.framework.BundleContext
import org.eclipse.core.runtime.Platform
import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.Path
import org.osgi.framework.Bundle
import org.eclipse.core.runtime.Status
import scala.util.control.ControlThrowable
import org.eclipse.core.runtime.IStatus
import java.net.URL

object Activator {
  val PLUGIN_ID: String = "org.zaluum.nide"
  var plugin: Activator = _
  def logError(s: String, a: AnyRef) {
    println("ERROR" + s + " " + a)
  }
  def getDefault(): Activator = {
    return plugin;
  }
}

class Activator extends AbstractUIPlugin {
  def pluginId = Activator.PLUGIN_ID
  override def start(context: BundleContext) = {
    super.start(context);
    Activator.plugin = this;
    
  }
  def bundleToPath(bundle: Bundle) = {
    if (bundle==null) None else {
      val rootEntry = {
        if (bundle.getEntry("/org") == null) 
          bundle.getEntry("/bin/")
        else
          bundle.getEntry("/")
      }
      Option(rootEntry) map { x => Path.fromOSString(FileLocator.toFileURL(x).getPath)}
    }
  }
  val zaluumLib = "ZALUUM_CONTAINER"
  val zaluumLibId = "org.zaluum." + zaluumLib 
  def libZaluumRuntimeBundle = Platform.getBundle("org.zaluum.runtime")
  def libZaluumRuntime = bundleToPath(libZaluumRuntimeBundle)
  def libZaluumRuntimeSrc = bundleToPath(Platform.getBundle("org.zaluum.runtime.source"))

  /*def runtimeJars = {
    val en = getBundle.findEntries("libs/runtime", "*.jar", false).asInstanceOf[java.util.Enumeration[URL]]
    //println("runtime jars " + en)
    import scala.collection.JavaConversions._
    for (
      enum ← Option(en).toList;
      e ← enum;
      val f = e.getFile;
      if (!f.contains(".source_") && !f.contains("javadoc"))
    ) yield {
      FileLocator.resolve(e)
    }
  }*/


  override def initializeImageRegistry(reg: ImageRegistry) = {
    super.initializeImageRegistry(reg);
    /*Utils.loadIcons(reg,classOf[Icons],"zaluum","stickynote", "composed", "const", "value", "instance", "portin", "portout", "wire");
    Utils.loadImage(reg,"portInD", classOf[Icons]);
    Utils.loadImage(reg,"portInND", classOf[Icons]);
    Utils.loadImage(reg,"portOutND", classOf[Icons]);*/
  }

  override def getImageRegistry(): ImageRegistry = {
    return super.getImageRegistry();
  }
  def logError(t : Throwable) : Unit = logError(t.getClass + ":" + t.getMessage, t)
  
  def logError(msg : String, t : Throwable) : Unit = {
    val t1 = if (t != null) t else { val ex = new Exception ; ex.fillInStackTrace ; ex }
    val status1 = new Status(IStatus.ERROR, pluginId, IStatus.ERROR, msg, t1)
    getLog.log(status1)

    val status = t match {
      case ce : ControlThrowable =>
        val t2 = { val ex = new Exception ; ex.fillInStackTrace ; ex }
        val status2 = new Status(
          IStatus.ERROR, pluginId, IStatus.ERROR,
          "Incorrectly logged ControlThrowable: "+ce.getClass.getSimpleName+"("+ce.getMessage+")", t2)
        getLog.log(status2)
      case _ =>
    }
  }
  
  final def check[T](f : => T) =
    try {
      Some(f)
    } catch {
      case e : Throwable =>
        logError(e)
        None
    }
}
