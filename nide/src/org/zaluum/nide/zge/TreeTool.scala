package org.zaluum.nide.zge

import org.zaluum.nide.newcompiler.PortDir
import org.zaluum.nide.newcompiler.In
import org.zaluum.nide.newcompiler.PortRef
import org.zaluum.nide.newcompiler.ConnectionDef
import org.zaluum.nide.newcompiler.ThisRef
import org.zaluum.nide.newcompiler.ValRef
import org.zaluum.nide.newcompiler.CopyTransformer
import org.zaluum.nide.newcompiler.BoxDef
import org.zaluum.nide.newcompiler.PortDef
import org.zaluum.nide.newcompiler.EmptyTree
import org.zaluum.nide.newcompiler.Name
import org.zaluum.nide.newcompiler.ValDef
import org.zaluum.nide.newcompiler.BoxTypeSymbol
import org.zaluum.nide.newcompiler.Tree

import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }

import scala.collection.JavaConversions._

class TreeTool(val viewer: TreeViewer) extends ItemTool(viewer) {
  def tree = viewer.tree
  def currentBoxDefLayers = current.asInstanceOf[BoxDefContainer] // FIXME

  override lazy val selecting = new Selecting {
    override def connect(port: PortFigure) {
      connecting.enter(initDrag, initContainer, port)
    }
    override def menu() {
      figureUnderMouse match {
        case Some(p: PortDeclFigure) ⇒ new PortDeclPopup(viewer, p).show(swtMouseLocation) // TODO Dispose?
        case Some(o: OpenBoxFigure) => println("openbox")
        case Some(b: ImageValFigure) ⇒
        case _ ⇒ viewer.palette.show(swtMouseLocation, current)
      }
    }
  }
  abstract class InnerCreating extends ToolState {
    self : SingleContainer =>
    var feed: ItemFeedbackFigure = _
    def enter(initContainer: BoxDefContainer) {
      enterSingle(initContainer)
      state = this
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, 48, 48))
      feed.show()
    }
    def move() { feed.setInnerLocation(currentMouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = MPoint(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new CopyTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val sym = b.symbol.asInstanceOf[BoxTypeSymbol]
            val name = Name(sym.freshName("box"))
            val className = Name(sym.freshName("C"))
            val internalTestVal = ValDef(Name("hola"), Name("fail"), MPoint(10, 10), None,None,None)
            val newDef = BoxDef(className, None, List(),
              vals = List(internalTestVal),
              ports = List(PortDef(Name("p"),Name("D"),In, MPoint(0,0), MPoint(0,10))), 
              connections = List())
            val newVal = ValDef(name, className, dst, Some(Dimension(200,200)), None,None)   
            b.copy(defs =  newDef :: b.defs, vals = newVal :: b.vals)
        }
      }
      controller.exec(TreeCommand(tr))
    }
    def buttonDown() {}
    def exit() {
      feed.hide()
      selecting.enter()
    }
  }
  object innercreating extends InnerCreating with SingleContainer with Allower // inherit
  abstract class Creating extends ToolState {
    self: SingleContainer =>
    var feed: ItemFeedbackFigure = _
    var tpe: BoxTypeSymbol = _
    def enter(tpe: BoxTypeSymbol, initContainer: BoxDefContainer) {
      enterSingle(initContainer)
      this.tpe = tpe
      state = this
      val img = viewer.imageFactory(tpe.decl);
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      feed.show()
    }
    def move() { feed.setInnerLocation(currentMouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = MPoint(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new CopyTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val name = Name(b.symbol.asInstanceOf[BoxTypeSymbol].freshName("box"))
            b.copy(vals = ValDef(name, tpe.name, dst, None,None,None) :: b.vals)
        }
      }
      controller.exec(TreeCommand(tr))
    }
    def buttonDown() {}
    def exit() {
      feed.hide();
      feed = null;
      selecting.enter()
    }
  }
  // CREATING BOX 
  object creating extends Creating with SingleContainer with Allower
  // CREATING PORT
  class CreatingPort extends ToolState {
    self : SingleContainer =>
    def enter(dir: PortDir, initContainer: BoxDefContainer) {
      enterSingle(initContainer)
      state = this
      this.dir = dir
      val img = viewer.imageFactory.get(PortDeclFigure.img(dir)).get
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      feed.show()
    }
    var feed: ItemFeedbackFigure = _
    var dir: PortDir = In
    //def initContainer : BoxDefContainer
    def move() { feed.setInnerLocation(currentMouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      val pos = MPoint(currentMouseLocation.x, currentMouseLocation.y)
      val tr = new CopyTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == initContainer.boxDef ⇒
            val name = Name(tree.symbol.asInstanceOf[BoxTypeSymbol].freshName("port"))
            val p = PortDef(name, Name("D"), dir, pos, MPoint(0, 0))
            b.copy(ports = p :: b.ports)
        }
      }
      controller.exec(TreeCommand(tr))
    }
    def buttonDown() {}
    def exit() { feed.hide(); feed = null; selecting.enter() }
  }
  object creatingPort extends CreatingPort with SingleContainer with Allower
  // CONNECT
  object connecting extends DeltaMove with SingleContainer{
    var dst: Option[PortFigure] = None
    var src: Option[PortFigure] = None
    val painter = new ConnectionPainter(currentBoxDefLayers)
    //var con: Option[Connection] = None
    val portsTrack = new OverTrack[PortFigure] {
      def container = current.portsLayer
      def onEnter(p: PortFigure) { dst = Some(p); p.showFeedback }
      def onExit(p: PortFigure) { dst = None; p.hideFeedback }
    }
    def enter(initdrag: Point, initContainer: BoxDefContainer, initPort: PortFigure) {
      super.enterMoving(initdrag)
      super.enterSingle(initContainer)
      src = Some(initPort)
      viewer.setCursor(Cursors.HAND)
      move()
    }
    def doEnter {}
    def buttonUp {
      // execute model command
      if (dst.isDefined) {
        def toRef(pf: PortFigure) = pf.valSym.map { s ⇒ ValRef(s.name) } getOrElse { ThisRef }
        val srcPortName = src.get.sym.name
        val dstPortName = dst.get.sym.name
        val srcRef = toRef(src.get)
        val dstRef = toRef(dst.get)
        if (srcRef != dstRef && srcPortName != dstPortName) {
          val condef = ConnectionDef(
            PortRef(srcRef, srcPortName, src.get.in),
            PortRef(dstRef, dstPortName, dst.get.in))
          controller.exec(TreeCommand(
            new CopyTransformer {
              val trans: PartialFunction[Tree, Tree] = {
                case b: BoxDef if (b == initContainer.boxDef) ⇒ b.copy(connections = condef :: b.connections)
              }
            }))
        } else exit()
      } else {
        exit()
      }
    }
    def drag {}
    def buttonDown {}
    def exit() {
      painter.clear
      dst foreach { _.hideFeedback }
      dst = None
      viewer.setCursor(null)
      selecting.enter()
    }
    def move() {
      val start = src.get.anchor
      val end = dst match {
        case Some(df) ⇒ df.anchor
        case None ⇒ currentMouseLocation
      }
      painter.paintRoute(Route(start, end))
      portsTrack.update()
    }
    def abort() { exit() }
  }
}
