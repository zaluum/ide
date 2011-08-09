package org.zaluum.nide.zge

import org.eclipse.jface.resource.DeviceResourceDescriptor
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.Ellipse
import org.eclipse.jface.viewers.ICellEditorListener
import org.eclipse.swt.widgets.Text
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.draw2d.text.TextFlow
import org.eclipse.draw2d.text.FlowPage
import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{ ColorConstants, Figure, ImageFigure, Graphics }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.{ Image, Font, GC }
import org.zaluum.nide.compiler._
import org.eclipse.swt.widgets.{ Composite, Display, Shell, Listener, Event }
import javax.swing.JComponent
import javax.swing.JPanel
import java.awt.{ Graphics ⇒ AG }
import java.awt.image.BufferedImage
import org.eclipse.draw2d.FigureUtilities
import org.zaluum.nide.Activator
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.zaluum.nide.eclipse.integration.model.MethodUtils
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding

// TREE SPECIFIC FIGURES
trait ValDefItem extends Item {
  var valDef: ValDef = _
  def valSym = valDef.sym
  def pos = valDef.pos
  override def selectionSubject = Some(valDef)
  def updateValDef(t: ValDef) {
    valDef = t
    updateMe()
    updateSize()
    updateValPorts()
  }
  def updateMe()
  def updateValPorts()
}

trait ValFigure extends ValDefItem with HasPorts {
  def sym = valDef.sym
  def myLayer = container.layer
  def updateValPorts() {
    for (p ← ports) container.portsLayer.remove(p)
    ports.clear

    val bports = sym.portSides;
    val (unsortedins, unsortedouts) = bports.partition { _.inPort } // SHIFT?
    val ins = unsortedins.toList.sortBy(_.name.str);
    val outs = unsortedouts.toList.sortBy(_.name.str);
      def space(s: PortSide) = if (s.inPort) size.h / (ins.size + 1) else size.h / (outs.size + 1)
      def createPort(s: PortSide, i: Int) {
        val p = new PortFigure(container)
        val x = if (s.inPort) 0 else size.w
        val point = Point(x, (i + 1) * space(s))
        p.update(point + Vector2(getBounds.x, getBounds.y), s)
        ports += p
      }
    for ((p, i) ← ins.zipWithIndex) createPort(p, i)
    for ((p, i) ← outs.zipWithIndex) createPort(p, i)

  }
}
class LabelItem(val container: ContainerItem, gui: Boolean = false) extends TextEditFigure with ValDefItem with RectFeedback {
  setForegroundColor(Colorizer.color(null))
  def blink(b: Boolean) {}
  def size = pg.getPreferredSize
  def baseVector = Vector2(0, -size.h)
  def lbl = if (gui) valDef.labelGui else valDef.label
  def text = {
    val fromTree = lbl.map(_.description).getOrElse("XXX")
    if (fromTree == "") valDef.name.str else fromTree
  }
  def basePos = if (gui) valDef.guiPos.getOrElse(Point(0, 0)) else valDef.pos
  override def pos = basePos + baseVector + (lbl.map { _.pos } getOrElse { Vector2(0, 0) })
  def myLayer = container.layer
  def updateMe() {
    fl.setText(text)
    pg.setBounds(new Rectangle(new EPoint(0, 0), dimension(size)))
  }
  def updateValPorts() {}
}
trait AutoDisposeImageFigure extends ImageFigure {
  var desc: DeviceResourceDescriptor = null
  def imageFactory: ImageFactory
  def disposeImage() {
    if (desc != null)
      imageFactory.destroy(desc)
  }
}
class ThisOpValFigure(container: ContainerItem) extends ImageValFigure(container) {
  private def jproject = container.viewer.zproject.jProject.asInstanceOf[JavaProject]
  override def updateValPorts() {
    super.updateValPorts()
    sym.info match {
      case m: MethodBinding ⇒
        MethodUtils.findMethodParamNames(m, jproject).foreach { names ⇒
          names.zipWithIndex.foreach {
            case (name, i) ⇒
              ports filter { _.ps.name.str == "p" + i } foreach { // remove literal "p"
                _.nameOverride = name
              }
          }
        }
      case f: FieldBinding ⇒
        val opi = sym.tpe match {
          case r: ResultExprType ⇒ Some(r.outPort(sym))
          case o: OneParameter   ⇒ Some(o.aPort(sym))
          case _                 ⇒ None
        }
        for (pi ← opi; port ← ports.find(_.ps.pi == pi))
          port.nameOverride = f.name.mkString
      case _ ⇒
    }
  }
  override def img = {
    sym.info match {
      case m: MethodBinding ⇒
        val txt =
          if (m.isConstructor())
            "new " + m.declaringClass.compoundName.last.mkString
          else {
            (if (m.isStatic()) m.declaringClass.compoundName.last.mkString else "") +
              "." + m.selector.mkString + "()"
          }
        imageFactory.invokeImage(txt)
      case f: FieldBinding ⇒
        val prefix = if (f.isStatic()) f.declaringClass.compoundName.last.mkString else ""
        imageFactory.invokeImage(prefix + "." + f.name.mkString)
      case _ ⇒
        val str = sym.tpe match {
          case NewArrayExprType=>
            val pi = NewArrayExprType.thisPort(sym)
            pi.finalTpe match {
              case tpe:ArrayType => Some("new " + tpe.of.name.str.split('.').last + "[]"*tpe.dim)
              case _ => None
            }
          case _ => None 
        }
        str match {
          case Some(str) => imageFactory.invokeImage(str)
          case None => imageFactory.invokeImageError("right click me")
        }
    }
  }
}
class ImageValFigure(val container: ContainerItem) extends AutoDisposeImageFigure with ValFigure with RectFeedback {
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
  def imageFactory = container.viewer.zproject.imageFactory
  def img = imageFactory(valDef.tpe)
  def updateMe() {
    disposeImage()
    val (newImg, newDesc) = img
    desc = newDesc
    setImage(newImg)
  }
  override def paintFigure(gc: Graphics) {
    gc.setAlpha(if (blinkOn) 100 else 255);
    super.paintFigure(gc)
  }
  var blinkOn = false
  def blink(b: Boolean) {
    blinkOn = b
    repaint()
  }
}
class DirectValFigure(val container: ContainerItem) extends RectangleFigure with TextEditFigure with ValFigure with RectFeedback {
  def size = {
    pg.getPreferredSize().ensureMin(Dimension(Tool.gridSize * 3, Tool.gridSize * 3)) + Vector2(Tool.gridSize, 0)
  }
  def param = valDef.params.headOption.asInstanceOf[Option[Param]]
  def text = param.map { _.value }.getOrElse { "0" }
  def updateMe {
    fl.setText(text)
    setForegroundColor(Colorizer.color(param.map(_.tpe).getOrElse(null)))
    pg.setBounds(new Rectangle(new EPoint(2, 2), dimension(size)))
  }
  def blink(c: Boolean) {
    this.setXOR(c)
  }
}
trait TextEditFigure extends Item {
  def text: String;
  setFont(Activator.getDefault.directEditFont) // https://bugs.eclipse.org/bugs/show_bug.cgi?id=308964
  val pg = new FlowPage()
  pg.setForegroundColor(ColorConstants.black)
  val fl = new TextFlow()
  pg.add(fl)
  add(pg)

  var textCellEditor: TextCellEditor = null
  def edit(onComplete: (String) ⇒ Unit, onCancel: () ⇒ Unit) = {
    if (textCellEditor == null) {
      textCellEditor = new TextCellEditor(container.viewer.canvas)
      val textC = textCellEditor.getControl().asInstanceOf[Text]
      textC.setFont(Activator.getDefault.directEditFont)
      textC.setText(text)
      textCellEditor.activate()
      textCellEditor.addListener(new ICellEditorListener() {
        def applyEditorValue() { onComplete(textC.getText) }
        def cancelEditor() { onCancel() }
        def editorValueChanged(oldValid: Boolean, newValid: Boolean) {}
      })
      val b = getClientArea.getCopy
      translateToAbsolute(b)
      textC.setBounds(b.x + 1, b.y + 1, math.max(b.width - 1, Tool.gridSize * 8), b.height - 2)
      textC.setBackground(ColorConstants.white)
      textC.setVisible(true)
      textC.selectAll()
      textC.setFocus
    }
  }
  def hideEdit() = {
    if (textCellEditor != null) {
      textCellEditor.dispose()
      textCellEditor = null
    }
  }
}
class SwingFigure(val container: ContainerItem, val cl: ClassLoader) extends ValDefItem with ResizableFeedback {
  setOpaque(true)
  def size = valDef.guiSize getOrElse { Dimension(Tool.gridSize * 5, Tool.gridSize * 5) }
  override def pos = valDef.guiPos getOrElse { Point(0, 0) }
  def myLayer = container.layer
  var component: Option[JComponent] = None
  def forName(str: String): Option[Class[_]] = {
    try { Some(cl.loadClass(str)) }
    catch { case e: Exception ⇒ e.printStackTrace; None }
  }

  override def updateValDef(valDef: ValDef) = {
      def instance(cl: Class[_]) = {
        try {
          Some(cl.newInstance().asInstanceOf[JComponent])
        } catch { case e ⇒ e.printStackTrace; None }
      }
    component = valDef.tpe match {
      case bs: BoxTypeSymbol ⇒
        for (
          c ← bs.visualClass;
          if c.str != classOf[JPanel].getName;
          cl ← forName(c.str);
          i ← instance(cl)
        ) yield i
      case _ ⇒ None
    }
    super.updateValDef(valDef)
  }
  def updateMe() {}
  override def updateValPorts() {}
  override def selectionSubject = Some(valDef)
  var blinkOn = false
  def blink(b: Boolean) {
    blinkOn = b
    // raise the figure
    val p = getParent
    p.remove(this)
    p.add(this)
    repaint()
  }
  override def paintFigure(g: Graphics) {
    val rect = getClientArea()
    g.setXORMode(blinkOn)
    component match {
      case Some(c) ⇒
        val aimage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB)
        val ag = aimage.createGraphics
        c.setBounds(0, 0, rect.width, rect.height);
        c.doLayout
        c.paint(ag)
        val imageData = SWTUtils.convertAWTImageToSWT(aimage)
        val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(), imageData)
        g.drawImage(image, rect.x, rect.y)
        image.dispose()
        ag.dispose();
      case None ⇒
        g.setForegroundColor(ColorConstants.lightGray)
        g.fillRectangle(rect)
        g.setForegroundColor(ColorConstants.gray)
        val data = g.getFont.getFontData
        for (d ← data) {
          d.setHeight(rect.height / 2)
        }
        val font = new Font(Display.getCurrent, data)
        g.setFont(font)
        val dim = FigureUtilities.getStringExtents("?", font)
        g.drawText("?", rect.getCenter.x - dim.width / 2, rect.getCenter.y - dim.height / 2)
        font.dispose()
    }
    g.setForegroundColor(ColorConstants.lightGray)
    g.drawRectangle(rect.getCopy.expand(-1, -1))
  }
}