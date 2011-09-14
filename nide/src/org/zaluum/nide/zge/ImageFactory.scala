package org.zaluum.nide.zge

import org.eclipse.draw2d.ColorConstants
import org.eclipse.jface.resource.DeviceResourceDescriptor
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.resource.JFaceResources
import org.eclipse.jface.resource.LocalResourceManager
import org.eclipse.jface.resource.ResourceManager
import org.eclipse.swt.graphics.Color
import org.eclipse.swt.graphics.Device
import org.eclipse.swt.graphics.GC
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.graphics.TextLayout
import org.eclipse.swt.widgets.Control
import org.eclipse.swt.SWT
import org.zaluum.nide.compiler.BoxType
import org.zaluum.nide.compiler.BoxTypeSymbol
import org.zaluum.nide.compiler.In
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Out
import org.zaluum.nide.compiler.PortDir
import org.zaluum.nide.compiler.Shift
import org.zaluum.nide.compiler.JavaType
import org.zaluum.nide.eclipse.ZaluumProject
import org.zaluum.nide.icons.Icons
import org.zaluum.nide.Activator

class ImageFactory private (val zp: ZaluumProject, val rm: ResourceManager) {
  def this(zp: ZaluumProject) = this(zp, new LocalResourceManager(JFaceResources.getResources))
  def this(i: ImageFactory) = this(i.zp, new LocalResourceManager(i.rm))
  def this(i: ImageFactory, c: Control) = this(i.zp, new LocalResourceManager(i.rm, c))

  def notFound = ImageDescriptor.createFromFile(classOf[Icons], "notFound.png")
  def portDeclIn = ImageDescriptor.createFromFile(classOf[Icons], "portDeclIn.png")
  def portDeclOut = ImageDescriptor.createFromFile(classOf[Icons], "portDeclOut.png")
  def portDeclShift = ImageDescriptor.createFromFile(classOf[Icons], "portDeclShift.png")
  def buttonIfTrue = ImageDescriptor.createFromFile(classOf[Icons], "buttonIfTrue.png")
  def buttonIfFalse = ImageDescriptor.createFromFile(classOf[Icons], "buttonIfFalse.png")

  def portImg(dir: PortDir) = dir match {
    case In    ⇒ (rm.createImage(portDeclIn), portDeclIn)
    case Out   ⇒ (rm.createImage(portDeclOut), portDeclOut)
    case Shift ⇒ (rm.createImage(portDeclShift), portDeclShift)
  }
  def destroy(d: DeviceResourceDescriptor) = rm.destroy(d)
  def destroyAll() = rm.dispose();
  def image48(name: Name): (Image, DeviceResourceDescriptor) = iconFor(None, name, 48);
  def icon(tpe: JavaType, minY: Int): (Image, DeviceResourceDescriptor) = {
    tpe match {
      case b: BoxTypeSymbol ⇒ iconFor(b.image, b.fqName, minY)
      case b: BoxType       ⇒ iconFor(None, b.fqName, minY)
      case _                ⇒ imageForText("<missing>", minY, ColorConstants.red)
    }
  }
  def invokeIcon(str: String, miny: Int) = imageForText(str, miny, ColorConstants.blue)
  def invokeIconError(str: String, miny: Int) = imageForText(str, miny, ColorConstants.red)
  def image(d: ImageDescriptor) = rm.createImage(d)
  private def resourceToDescriptor(resource: String) =
    Option(zp.classLoader.getResource(resource)) map { ImageDescriptor.createFromURL }

  private def resourceToImage(resource: String): Option[(Image, ImageDescriptor)] =
    resourceToDescriptor(resource) flatMap { desc ⇒
      try {
        Some((rm.create(desc).asInstanceOf[Image], desc))
      } catch { case e ⇒ None }
    }
  private def nestedImageFor(imageName: Option[String], name: Name): Option[(Image, ImageDescriptor)] = {
    imageName.flatMap { imgName ⇒
      resourceToImage(imgName)
    }.orElse { resourceToImage(name.toRelativePath + ".png") }
  }
  private def iconFor(imageName: Option[String], name: Name, minY: Int) = {
    nestedImageFor(imageName, name) match {
      case Some((img, desc)) ⇒
        val newDesc = GeneratedImageIconImageDescriptor(desc, minY)
        val img = rm.create(newDesc).asInstanceOf[Image]
        destroy(desc)
        (img, desc)
      case None ⇒ imageForText(name.classNameWithoutPackage.str, minY, ColorConstants.black)

    }
  }
  private def imageForText(txt: String, ySize: Int, color: Color) = {
    val desc = GeneratedTextIconImageDescriptor(txt, ySize + 12, color)
    (rm.create(desc).asInstanceOf[Image], desc)
  }
  case class GeneratedImageIconImageDescriptor(desc: ImageDescriptor, minY: Int) extends DeviceResourceDescriptor {
    def createResource(device: Device): Object = {
      val nestedImg = image(desc)
      val nestedY = nestedImg.getBounds().height
      val nestedX = nestedImg.getBounds().width
      val y = if (nestedY >= minY) 0 else (minY - nestedY) / 2
      val height = math.max(nestedY, minY)
      val img = new Image(device, nestedX, height)
      val gc = new GC(img)
      gc.drawImage(nestedImg, 0, y)
      if (nestedY < minY) {
        gc.setForeground(ColorConstants.gray)
        gc.drawRectangle(0, 0, nestedX - 1, height - 1)
      }
      destroy(desc)
      gc.dispose
      img
    }

    def destroyResource(previouslyCreatedObject: Object) {
      previouslyCreatedObject.asInstanceOf[Image].dispose();
    }
  }
  case class GeneratedTextIconImageDescriptor(text: String, ySize: Int, color: Color) extends DeviceResourceDescriptor {
    def createResource(device: Device): Object = {
      val font = Activator.getDefault.generatedIconFont
      val t = new TextLayout(device)
      t.setText(text)
      t.setAlignment(SWT.CENTER)
      t.setFont(font)
      t.setWidth(47)
      val textY = t.getBounds.height
      val finalY = math.max(ySize, textY)
      val img = new Image(device, 48, finalY);
      val gc = new GC(img)
      gc.setForeground(color)
      gc.drawRectangle(0, 0, 47, finalY - 1)
      t.draw(gc, 0, math.max(0, (finalY - 1 - textY) / 2))
      t.dispose
      gc.dispose
      img
    }

    def destroyResource(previouslyCreatedObject: Object) {
      previouslyCreatedObject.asInstanceOf[Image].dispose();
    }
  }
}
