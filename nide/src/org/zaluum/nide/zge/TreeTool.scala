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
  var current: BoxDefLayers = viewer // TODO better init?
  //override def modelView = viewer.modelView
  override lazy val selecting = new Selecting {
    override def connect(port: PortFigure) {
      connecting.enter(initDrag, port)
    }
    override def menu() {
      figureUnderMouse match {
        case Some(p: PortDeclFigure) ⇒ new PortDeclPopup(viewer, p).show(swtMouseLocation) // TODO Dispose?
        case Some(b: BoxFigure) ⇒
        case _ ⇒ viewer.palette.show(swtMouseLocation)
      }
    }
  }
  object innercreating extends ToolState { // inherit
    var feed: ItemFeedbackFigure = _
    def enter() {
      state = this
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, 48, 48))
      feed.show()
    }
    def move() { feed.setInnerLocation(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = MPoint(mouseLocation.x, mouseLocation.y)
      val tr = new CopyTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == tree ⇒
            val sym = b.symbol.asInstanceOf[BoxTypeSymbol]
            val name = Name(sym.freshName("box"))
            val className = Name(sym.freshName("C"))
            val testVal = ValDef(Name("hola"),Name("fail"),MPoint(10,10),EmptyTree)
            b.copy(defs = BoxDef(className,None,List(),
                            vals=List(testVal),
                            List(),List()) :: b.defs,
                vals = ValDef(name, className, dst, EmptyTree) :: b.vals)
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
  // CREATING BOX 
  object creating extends ToolState {
    var feed: ItemFeedbackFigure = _
    var tpe: BoxTypeSymbol = _
    def enter(tpe: BoxTypeSymbol) {
      this.tpe = tpe
      state = this
      val img = viewer.imageFactory(tpe.decl);
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      feed.show()
    }
    def move() { feed.setInnerLocation(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      val dst = MPoint(mouseLocation.x, mouseLocation.y)
      val tr = new CopyTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == tree ⇒
            val name = Name(b.symbol.asInstanceOf[BoxTypeSymbol].freshName("box"))
            b.copy(vals = ValDef(name, tpe.name, dst, EmptyTree) :: b.vals)
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
  // CREATING PORT
  object creatingPort extends ToolState {
    var feed: ItemFeedbackFigure = _
    var dir: PortDir = In
    def enter(dir: PortDir) {
      state = this
      this.dir = dir
      val img = viewer.imageFactory.get(PortDeclFigure.img(dir)).get
      feed = new ItemFeedbackFigure(current)
      feed.setInnerBounds(new Rectangle(0, 0, img.getBounds.width, img.getBounds.height));
      feed.show()
    }
    def move() { feed.setInnerLocation(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      val pos = MPoint(mouseLocation.x, mouseLocation.y)
      val tr = new CopyTransformer() {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if b == tree ⇒
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
  // CONNECT
  object connecting extends MovingState {
    var dst: Option[PortFigure] = None
    var src: Option[PortFigure] = None
    val painter = new ConnectionPainter(current)
    //var con: Option[Connection] = None
    val portsTrack = new OverTrack[PortFigure](viewer.portsLayer) {
      def onEnter(p: PortFigure) { dst = Some(p); p.showFeedback }
      def onExit(p: PortFigure) { dst = None; p.hideFeedback }
    }
    def enter(initdrag: Point, initPort: PortFigure) {
      super.enter(initdrag)
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
                case b: BoxDef if (b == tree) ⇒ b.copy(connections = condef :: b.connections)
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
        case None ⇒ mouseLocation
      }
      painter.paintRoute(Route(start, end))
      portsTrack.update()
    }
    def abort() { exit() }
  }
}

