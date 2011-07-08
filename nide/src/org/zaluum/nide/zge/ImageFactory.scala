package org.zaluum.nide.zge

import org.zaluum.nide.Activator
import org.eclipse.ui.PlatformUI
import org.eclipse.core.runtime.Platform
import org.eclipse.swt.graphics.TextLayout
import org.eclipse.swt.widgets.Control
import org.eclipse.jface.resource.LocalResourceManager
import org.eclipse.swt.graphics.Device
import org.eclipse.jface.resource.DeviceResourceDescriptor
import org.eclipse.jface.resource.JFaceResources
import org.eclipse.jface.resource.ResourceManager
import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.geometry.Rectangle
import org.eclipse.draw2d.IFigure
import org.eclipse.draw2d.AbstractBackground
import org.eclipse.draw2d.Graphics
import org.eclipse.draw2d.Figure
import org.zaluum.nide.eclipse.ClassPath
import org.zaluum.nide.compiler._
import org.eclipse.jface.dialogs.PopupDialog
import org.eclipse.jface.resource.{ ImageRegistry, ImageDescriptor }
import org.eclipse.swt.SWT
import org.eclipse.swt.custom.ScrolledComposite
import org.eclipse.swt.graphics.{ Image, GC, Font, Point }
import org.eclipse.swt.layout.{ GridLayout, FillLayout }
import org.eclipse.swt.widgets.{ Display, Shell, Composite }
import org.zaluum.nide.icons.Icons
import org.zaluum.nide.eclipse.ZaluumProject
import org.eclipse.core.resources.IProject

class ImageFactory private (val zp: ZaluumProject, val rm: ResourceManager) {
  def this(zp: ZaluumProject) = this(zp, new LocalResourceManager(JFaceResources.getResources))
  def this(i: ImageFactory) = this(i.zp, new LocalResourceManager(i.rm))
  def this(i: ImageFactory,c:Control) = this(i.zp, new LocalResourceManager(i.rm,c))
  
  def notFound = ImageDescriptor.createFromFile(classOf[Icons], "notFound.png")
  def portDeclIn = ImageDescriptor.createFromFile(classOf[Icons], "portDeclIn.png")
  def portDeclOut = ImageDescriptor.createFromFile(classOf[Icons], "portDeclOut.png")
  def portDeclShift = ImageDescriptor.createFromFile(classOf[Icons], "portDeclShift.png")
  def portImg(dir: PortDir) = dir match {
    case In ⇒ (rm.createImage(portDeclIn), portDeclIn)
    case Out ⇒ (rm.createImage(portDeclOut), portDeclOut)
    case Shift ⇒ (rm.createImage(portDeclShift), portDeclShift)
  }
  def destroy(d: DeviceResourceDescriptor) = rm.destroy(d)
  def destroyAll() = rm.dispose();
  def apply(name: Name): (Image, DeviceResourceDescriptor) = imageFor(None, name);
  def apply(tpe: Type): (Image, DeviceResourceDescriptor) = {
    tpe match {
      case b: BoxTypeSymbol ⇒ imageFor(b.image, b.name)
      case b: BoxType ⇒ imageFor(None, b.name)
      case _ ⇒ (rm.createImage(notFound), notFound)
    }
  }
  def apply(typeTree: Tree): (Image, DeviceResourceDescriptor) = {
    typeTree match {
      case b: BoxDef ⇒ imageFor(b.image, b.name)
      case _ ⇒ (rm.createImage(notFound), notFound)
    }
  }
  private def resourceToDescriptor(resource: String) =
    zp.getResource(resource) map { ImageDescriptor.createFromURL }

  private def resourceToImage(resource: String): Option[(Image, DeviceResourceDescriptor)] =
    resourceToDescriptor(resource) flatMap { desc ⇒
      try {
        Some((rm.create(desc).asInstanceOf[Image], desc))
      } catch { case e ⇒ None }
    }

  private def imageFor(imageName: Option[String], name: Name): (Image, DeviceResourceDescriptor) = {
    imageName flatMap { resourceToImage } orElse {
      resourceToImage(name.toRelativePath + ".png")
    } getOrElse {
      val desc = GeneratedIconImageDescriptor(name)
      (rm.create(desc).asInstanceOf[Image], desc)
    }
  }
  case class GeneratedIconImageDescriptor(name: Name) extends DeviceResourceDescriptor {
    def createResource(device: Device): Object = {
      val img = new Image(device, 48, 48);
      val gc = new GC(img)
      val t = new TextLayout(device)
      val font = Activator.getDefault.generatedIconFont
      t.setText(name.classNameWithoutPackage)
      t.setAlignment(SWT.CENTER)
      t.setFont(font)
      t.setWidth(47)
      gc.drawRectangle(0, 0, 47, 47)
      t.draw(gc,0,math.max(0,(47-t.getBounds.height)/2))
      t.dispose
      gc.dispose
      img
    }

    def destroyResource(previouslyCreatedObject: Object) {
      previouslyCreatedObject.asInstanceOf[Image].dispose();
    }
  }
}
