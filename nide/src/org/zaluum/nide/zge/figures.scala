package org.zaluum.nide.zge

import org.eclipse.draw2d.Shape
import org.eclipse.swt.graphics.Color
import org.eclipse.draw2d.Ellipse
import org.eclipse.swt.events.FocusListener
import org.eclipse.jface.viewers.ICellEditorListener
import org.eclipse.swt.widgets.Text
import org.eclipse.jface.viewers.TextCellEditor
import org.eclipse.draw2d.text.TextFlow
import org.eclipse.draw2d.text.FlowPage
import org.eclipse.draw2d.RectangleFigure
import draw2dConversions._
import org.eclipse.draw2d.{ ColorConstants, Figure, ImageFigure, Polyline, Graphics }
import org.eclipse.draw2d.geometry.{ Rectangle, Point ⇒ EPoint, Dimension ⇒ EDimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Image
import org.zaluum.nide.compiler._
import scala.collection.mutable.Buffer
import org.eclipse.swt.widgets.{ Composite, Display, Shell, Listener, Event }
import javax.swing.JButton
import javax.swing.JComponent
import java.awt.{ Graphics ⇒ AG }
import java.awt.image.BufferedImage

// TREE SPECIFIC FIGURES
trait ValFigure extends SimpleItem with TreeItem {
  type T = ValDef
  def sym = tree.symbol.asInstanceOf[ValSymbol]
  def pos = tree.pos
  def myLayer = container.layer
  def container: BoxDefContainer
  def populateFigures() {
    val l = sym.tpe match {
      case b: BoxTypeSymbol ⇒
        b.ports.values.collect {
          case s: PortSymbol ⇒
            new PortFigure(s.extPos + Vector2(getBounds.x, getBounds.y),
              s, s.dir == In, Some(sym), container)
        }.toList
      case _ ⇒ List()
    }
    helpers.appendAll(l);
  }
}
class ImageValFigure(val tree: ValDef, val container: BoxDefContainer) extends ImageFigure with ValFigure with RectFeedback {
  def size = Dimension(getImage.getBounds.width, getImage.getBounds.height)
  override def show(){
    setImage(container.viewerResources.imageFactory(tree.tpe))
    super.show()
  }
}
class DirectValFigure(val tree: ValDef, val param: Param, val container: BoxDefContainer) extends TextEditFigure with ValFigure {
  def size = Dimension(40, 20)
  def text = param.value
  override def populateFigures() {
    fl.setText(text)
    setForegroundColor(Colorizer.color(param.tpe))
    super.populateFigures()
  }
}
trait TextEditFigure extends RectangleFigure with SimpleItem with RectFeedback {
  def text: String;

  val pg = new FlowPage()
  pg.setForegroundColor(ColorConstants.black)
  pg.setBounds(new Rectangle(2, 2, 40, 20))
  val fl = new TextFlow()
  pg.add(fl)
  add(pg)

  var textCellEditor: TextCellEditor = null
  def edit(onComplete: (String) ⇒ Unit, onCancel: () ⇒ Unit) = {
    if (textCellEditor == null) {
      textCellEditor = new TextCellEditor(container.viewer.canvas)
      val textC = textCellEditor.getControl().asInstanceOf[Text]
      textC.setText(text)
      textCellEditor.activate()
      textCellEditor.addListener(new ICellEditorListener() {
        def applyEditorValue() { onComplete(textC.getText) }
        def cancelEditor() { onCancel() }
        def editorValueChanged(oldValid: Boolean, newValid: Boolean) {}
      })
      val b = getClientArea.getCopy
      translateToAbsolute(b)
      textC.setBounds(b.x + 1, b.y + 1, b.width - 2, b.height - 2)
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
class SwingFigure(val container: Container, val box: ValDef, val component: JComponent) extends SimpleItem with TreeItem with ResizableFeedback {
  setOpaque(true)
  type T = ValDef
  def tree = box
  def size = box.guiSize getOrElse { Dimension(15, 15) }
  def pos = box.guiPos getOrElse { Point(0, 0) }
  def myLayer = container.layer
  def populateFigures {}
  override def paintFigure(g: Graphics) {
    val rect = getClientArea()
    component.setBounds(0, 0, rect.width, rect.height);
    component.doLayout
    val aimage = new BufferedImage(rect.width, rect.height, BufferedImage.TYPE_INT_RGB)
    val ag = aimage.createGraphics
    component.paint(ag)
    val imageData = SWTUtils.convertAWTImageToSWT(aimage)
    val image = new org.eclipse.swt.graphics.Image(Display.getCurrent(), imageData)
    g.drawImage(image, rect.x, rect.y)
    ag.dispose();
    image.dispose()
  }
}