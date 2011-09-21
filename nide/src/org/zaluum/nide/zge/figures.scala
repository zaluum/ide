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
import org.zaluum.nide.utils.MethodBindingUtils
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.ui.views.properties.TextPropertyDescriptor
import org.eclipse.ui.views.properties.IPropertyDescriptor
import org.eclipse.ui.views.properties.IPropertySource2
import org.zaluum.nide.utils.SwingSWTUtils._
import org.eclipse.ui.views.properties.ColorPropertyDescriptor
import org.eclipse.swt.graphics.RGB
import org.eclipse.ui.views.properties.PropertyDescriptor
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.swt.graphics.FontData
import org.eclipse.jface.viewers.DialogCellEditor
import org.eclipse.swt.widgets.FontDialog
import org.eclipse.swt.widgets.Control
import org.zaluum.nide.compiler.InvalidValueType
import org.zaluum.nide.utils.SWTScala
import org.zaluum.nide.utils.SwingSWTUtils
import org.zaluum.nide.eclipse.ImageFactory

// TREE SPECIFIC FIGURES
trait ValDefItem extends Item with PropertySource {
  var valDef: ValDef = _
  def valSym = valDef.sym
  def pos = valDef.pos
  import RichFigure._
  def viewPortPos = this.translateToViewport(pos)
  override def selectionSubject = Some(valDef)
  def updateValDef(t: ValDef) {
    valDef = t
    updateMe()
    updateSize()
    updateValPorts()
    properties = calcProperties()
  }
  def updateMe()
  def updateValPorts()
  def controller = container.viewer.controller
  def calcProperties(): List[Property] = {
    val tpe = new ValDefTypeProperty(valDef, controller)
    val nme = new NameProperty(valDef, controller)
    val lbl = new LabelProperty(valDef, controller, false)
    val lblGui = new LabelProperty(valDef, controller, true)
    // FIXME val cons = new ConstructorSelectProperty(valDef, controller)
    val props: List[ParamProperty] = valSym.tpe match {
      case p: PropertySourceType ⇒ p.properties(controller, valDef)
      case _                     ⇒ List()
    }
    val missing = valDef.params filter {
      case p: Param ⇒ !props.exists { _.key == p.key }
    } map { case p: Param ⇒ new MissingParamProperty(controller, p, valDef) }
    nme :: tpe :: lbl :: lblGui :: /*FIXME cons :: */ missing ::: props
  }
  def display = container.viewer.display
}

trait ValFigure extends ValDefItem with HasPorts {
  def sym = valDef.sym
  def myLayer = container.layer
  var ins = List[PortSide]()
  var outs = List[PortSide]()
  var minYSize = 0
  val separation = 6
  def updateMe() {
    val bports = sym.portSides;
    val (unsortedins, unsortedouts) = bports.partition { _.inPort } // SHIFT?
    ins = unsortedins.toList.sortBy(_.name.str);
    outs = unsortedouts.toList.sortBy(_.name.str);
    val max = math.max(ins.size, outs.size)
    val correctedMax = if (max == 2) 3 else max
    // spread 2 ports like X _ X
    minYSize = correctedMax * separation
  }
  def updateValPorts() {
    val center = size.h / 2
    val insStartY = center - separation * (ins.size / 2)
    val outStartY = center - separation * (outs.size / 2)
      def createPort(s: PortSide, i: Int) {
        val p = new PortFigure(container)
        val x = if (s.inPort) 0 else size.w
        val sourceList = if (s.inPort) ins else outs
        val skipCenter = if (sourceList.size == 2 && i == 1) 1 else 0 // skip 1 position 
        val starty = if (s.inPort) insStartY else outStartY
        val point = Point(x, +starty + ((i + skipCenter) * separation))
        p.update(point + Vector2(getBounds.x, getBounds.y), s)
        ports += p
      }
    for (p ← ports) container.portsLayer.remove(p)
    ports.clear
    for ((p, i) ← ins.zipWithIndex) createPort(p, i)
    for ((p, i) ← outs.zipWithIndex) createPort(p, i)

  }
}
class LabelItem(val container: ContainerItem, gui: Boolean = false) extends TextEditFigure with ValDefItem with RectFeedback {
  setForegroundColor(Colorizer.color(null))
  def blink(b: Boolean) {}
  override val textPos = new EPoint(0, 0)
  def size = preferredTextSize
  def baseVector = Vector2(0, -size.h)
  def lbl = if (gui) valDef.labelGui else valDef.label
  def text = {
    val fromTree = lbl.map(_.description).getOrElse("XXX")
    if (fromTree == "") valDef.name.str else fromTree
  }
  def basePos = if (gui) valSym.bounds.map { r ⇒ Point(r.x, r.y) }.getOrElse(Point(0, 0)) else valDef.pos
  override def pos = basePos + baseVector + (lbl.map { _.pos } getOrElse { Vector2(0, 0) })
  def myLayer = container.layer
  def updateMe() {
    updateText()
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
        sym.portInstances
        imageFactory.invokeIcon(txt, minYSize)
      case f: FieldBinding ⇒
        val prefix = if (f.isStatic()) f.declaringClass.compoundName.last.mkString else ""
        imageFactory.invokeIcon(prefix + "." + f.name.mkString, minYSize)
      case _ ⇒
        val str = sym.tpe match {
          case NewArrayExprType ⇒
            val pi = NewArrayExprType.thisPort(sym)
            pi.tpe match {
              case tpe: ArrayType ⇒ Some("new " + tpe.of.name.str.split('.').last + "[]" * tpe.dim)
              case _              ⇒ None
            }
          case _ ⇒ None
        }
        str match {
          case Some(str) ⇒ imageFactory.invokeIcon(str, minYSize)
          case None      ⇒ imageFactory.invokeIconError("right click me", minYSize)
        }
    }
  }
}
class ImageValFigure(val container: ContainerItem) extends AutoDisposeImageFigure with ValFigure with RectFeedback {
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
  def imageFactory = container.viewer.zproject.imageFactory
  def img = imageFactory.icon(tpe, minYSize)

  def tpe = valDef.sym.tpe match {
    case BoxExprType ⇒ valDef.sym.classinfo
    case o           ⇒ o
  }
  override def updateMe() {
    super.updateMe()
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
class LiteralFigure(val container: ContainerItem) extends RectangleFigure with TextEditFigure with ValFigure with RectFeedback {
  def size = preferredSize
  def param = valDef.params.headOption.asInstanceOf[Option[Param]]
  def text = param.map { _.value }.getOrElse { "0" }
  override val textPos = new EPoint(2, 2)
  override def updateMe {
    super.updateMe()
    updateText()
    setForegroundColor(Colorizer.color(param.map(_.tpe).getOrElse(null)))
  }
  def blink(c: Boolean) {
    this.setXOR(c)
  }
}
trait TextEditFigure extends Item {
  def text: String;
  def preferredSize =
    preferredTextSize.ensureMin(Dimension(baseSpace * 3, baseSpace * 3)) + Vector2(baseSpace, 0)
  def preferredTextSize = pg.getPreferredSize()
  val textPos = new EPoint(2, 2)
  setFont(Activator.getDefault.directEditFont) // https://bugs.eclipse.org/bugs/show_bug.cgi?id=308964
  private val pg = new FlowPage()
  pg.setForegroundColor(ColorConstants.black)
  private val fl = new TextFlow()
  pg.add(fl)
  add(pg)
  def updateText() {
    fl.setText(text)
    pg.setBounds(new Rectangle(textPos, preferredTextSize))
  }
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
      textC.setBounds(b.x + 1, b.y + 1, math.max(b.width - 1, baseSpace * 8), b.height - 2)
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
class SwingFigure(val treeViewer: TreeViewer, val container: ContainerItem, val cloader: ClassLoader) extends ValDefItem with ResizableFeedback {
  setOpaque(true)
  def boundsValue = valSym.params.find {
    case (k, v) ⇒
      k.name == Name("bounds") &&
        v.valueTpe == RectangleValueType &&
        v.valid
  }.map {
    case (k, v) ⇒
      v.parse.asInstanceOf[java.awt.Rectangle]
  }
  def size =
    boundsValue map { r ⇒ Dimension(r.width, r.height) } getOrElse {
      Dimension(baseSpace * 5, baseSpace * 5)
    }
  var position = Point(0, 0)
  override def pos = position
  def myLayer = container.layer
  var component: Option[java.awt.Component] = None

  def updateMe() {
    position = boundsValue map { r ⇒ Point(r.x, r.y) } getOrElse {
      import RichFigure._
      treeViewer.findFigureOf(valDef) match {
        case Some(i) ⇒ i.viewPortPos
        case None    ⇒ Point(0, 0)
      }
    }
      def instance(cl: Class[_]) = {
        try {
          Some(cl.newInstance().asInstanceOf[java.awt.Component])
        } catch { case e ⇒ e.printStackTrace; None }
      }

    component = valDef.tpe match {
      case BoxExprType ⇒
        val cjt = valDef.sym.classinfo.asInstanceOf[ClassJavaType]
        for (
          cl ← cjt.loadClass(cloader);
          i ← instance(cl)
        ) yield i
      case _ ⇒ None
    }
    component foreach { c ⇒
      valSym.params.foreach {
        case (param: BeanParamSymbol, v) ⇒
          val classParam = param.tpe.loadClass(cloader)
          c.getClass().getMethods() find { m ⇒
            m.getName == param.setter.selector.mkString &&
              m.getParameterTypes.size == 1 &&
              Some(m.getParameterTypes()(0)) == classParam
          } foreach { m ⇒
            try {
              if (v.valid) {
                val parsed = v match {
                  case z: ZaluumParseValue ⇒
                    z.parse(container.viewer.controller.zproject).asInstanceOf[AnyRef]
                  case _ ⇒ v.parse.asInstanceOf[AnyRef]
                }
                m.invoke(c, parsed)
              }
            } catch { case e ⇒ e.printStackTrace() }
          }
        case _ ⇒
      }
    }
  }
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
    try {
      val rect = getClientArea()
      g.setXORMode(blinkOn)
      component match {
        case Some(c) ⇒
          val aimage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB)
          val ag = aimage.createGraphics
          c.setBounds(0, 0, rect.width, rect.height);
          c.doLayout
          c.paint(ag)
          val imageData = SwingSWTUtils.convertAWTImageToSWT(aimage)
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
    } catch {
      case e ⇒ e.printStackTrace
    }
    //g.setForegroundColor(ColorConstants.lightGray)
    //g.drawRectangle(rect.getCopy.expand(-1, -1))
  }
}