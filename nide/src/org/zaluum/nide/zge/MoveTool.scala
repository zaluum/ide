package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import scala.collection.JavaConversions._

class MoveTool(viewer: Viewer) extends Tool(viewer) {
  state = selecting
  // SELECTING
  object selecting extends ToolState {
    var selected: Option[BasicFigure] = None
    var lineSelected: Option[LineFigure] = None
    var handle: Option[HandleRectangle] = None
    var port: Option[PortFigure] = None
    var initDrag: Point = _
    def enter() { state = this }
    def buttonDown {
      selected = figureUnderMouse
      if (selected.isEmpty) lineSelected = lineUnderMouse
      initDrag = mouseLocation.getCopy
    }

    def buttonUp {
      (selected, lineSelected) match {
        case (Some(box), _) ⇒ modelView.selected.updateSelection(Set(box), shift)
        case (None, Some(line)) ⇒ modelView.selectedLines.updateSelection(Set(line), shift)
        case (None, None) ⇒ modelView.deselectAll()
      }
    }
    val handleTrack = new OverTrack[HandleRectangle](viewer.feedbackLayer) {
      def onEnter(h: HandleRectangle) {
        handle = Some(h)
        h.setXOR(true);
        viewer.setCursor(h.resizeCursor)
      }
      def onExit(f: HandleRectangle) {
        handle = None
        f.setXOR(false);
        viewer.setCursor(null)
      }
    }
    val portsTrack = new OverTrack[PortFigure](viewer.portsLayer) {
      def onEnter(p: PortFigure) { port = Some(p); p.showFeedback }
      def onExit(p: PortFigure) { port = None; p.hideFeedback }
    }
    def move {
      handleTrack.update()
      portsTrack.update()
    }
    def drag {
      (handle, selected, port) match {
        case (Some(h), _, _) ⇒ // resize
          resizing.enter(initDrag, h)
        case (None, _, Some(port)) ⇒ // connect
          connect.enter(initDrag, port)
        case (None, Some(fig), _) ⇒ // select and move
          if (!modelView.selected(fig))
            modelView.selected.updateSelection(Set(fig), shift)
          moving.enter(initDrag)
        case (None, None, None) ⇒ marqueeing.enter(initDrag) // marquee
      }
    }
    override def menu() {
      figureUnderMouse match {
        case Some(p: PortDeclFigure) ⇒ new PortDeclPopup(viewer, p).show(swtMouseLocation) // TODO Dispose?
        case Some(b: BoxFigure) ⇒
        case _ ⇒ viewer.palette.show(swtMouseLocation)
      }
    }
    def abort {}
    def exit {}
  }
  // CONNECT
  object connect extends MovingState {
    var dst: Option[PortFigure] = None
    var initPort: Option[PortFigure] = None
    var con: Option[Connection] = None
    var conf: Option[ConnectionFigure] = None
    val portsTrack = new OverTrack[PortFigure](viewer.portsLayer) {
      def onEnter(p: PortFigure) {
        dst = Some(p);
        con.get.to = Some(p.portRef);
        p.showFeedback
      }
      def onExit(p: PortFigure) { dst = None; con.get.to = None; p.hideFeedback }
    }
    def enter(initdrag: Point, initPort: PortFigure) {
      super.enter(initdrag)
      this.initPort = Some(initPort)
      con = Some(new Connection(Some(initPort.portRef), None))
      conf = Some(new ConnectionFigure(con.get, modelView))
      con.get.simpleConnect(initPort.getBounds.getCenter, mouseLocation)
      conf.get.show
      conf.get.update
      viewer.setCursor(Cursors.HAND)
    }
    def doEnter {}
    def buttonUp {
      // execute model command
      val command = new ConnectCommand(modelView.model, con.get)
      if (command.canExecute) {
        println("can execute")
        controller.exec(command)
      }
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() {
      dst foreach { _.hideFeedback }
      conf.foreach { _.hide }
      con = None
      conf = None
      viewer.setCursor(null)
      selecting.enter()
    }
    def move() {
      val end = dst match {
        case Some(df) ⇒ df.getBounds.getCenter
        case None ⇒ mouseLocation
      }
      con.get.simpleConnect(initPort.get.getBounds.getCenter, end)
      conf.get.update
      portsTrack.update()
    }
    def abort() { exit() }
  }
  // MOVE
  object moving extends MovingState {
    def doEnter {}
    def buttonUp {
      val commands = modelView.selected.selected map { bf ⇒
        val oldLoc = bf.getBounds.getLocation
        new MoveCommand(bf.positionable, oldLoc + delta)
      };
      controller.exec(new ChainCommand(commands.toList))
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { modelView.selected.selected foreach { _.moveDeltaFeed(delta) } }
    def abort() {
      modelView.selected.selected foreach { _.moveDeltaFeed(Vector2(0, 0)) }
      exit()
    }
  }
  /// MARQUEE
  object marqueeing extends MovingState {
    def doEnter() {
      viewer.setCursor(Cursors.CROSS)
      viewer.showMarquee()
      move()
    }
    def exit() {
      viewer.setCursor(null)
      selecting.enter()
    }
    def abort() { exit() }
    def buttonDown {}
    def drag {}
    def buttonUp {
      viewer.hideMarquee()
      exit()
    }
    def move { viewer.moveMarquee(new Rectangle(mouseLocation, initDrag)) }
  }
  // RESIZING
  object resizing extends MovingState {
    var handle: HandleRectangle = _
    def bf = handle.basicFigure

    def enter(initDrag: Point, handle: HandleRectangle) {
      super.enter(initDrag)
      this.handle = handle
    }
    def doEnter {}
    def buttonUp {
      val newBounds = handle.deltaAdd(delta, bf.getBounds);
      //val comm = new ResizeCommand(bf.box, (newBounds.x,newBounds.y), (newBounds.width,newBounds.height))
      //controller.exec(comm)
      exit()
    }
    def move() { bf.resizeDeltaFeed(delta, handle) }
    def abort() {
      bf.resizeDeltaFeed(Vector2(0, 0), handle)
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
  }
  // CREATING 
  object creating extends ToolState {
    var bf: BoxFigure = _
    def enter(boxClass: BoxClass) {
      state = this
      val name = model.nextFreeName("box")
      val box = new Box()
      box.className = boxClass.className
      box.name = name
      box.pos = MPoint(1, 1)
      bf = new ImageBoxFigure(box, Some(boxClass), viewer)
      bf.update()
      bf.hide()
      bf.showFeedback()
    }
    def move() { bf.moveFeed(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      bf.box.pos = MPoint(mouseLocation.x, mouseLocation.y)
      val com = new CreateCommand(bf.box, model)
      controller.exec(com)
      exit()
    }
    def buttonDown() {}
    def exit() { bf.hideFeedback; bf = null; selecting.enter() }
  }
  object creatingPort extends ToolState {
    var pf: PortDeclFigure = _
    def enter(in: Boolean) {
      state = this
      val name = model.nextFreeName("port")
      val portDecl = new PortDecl(model, name, in)
      portDecl.pos = MPoint(1, 1)
      pf = new PortDeclFigure(portDecl, viewer)
      pf.update()
      pf.hide()
      pf.showFeedback()
    }
    def move() { pf.moveFeed(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      // execute
      pf.portDecl.pos = MPoint(mouseLocation.x, mouseLocation.y)
      val com = new CreatePortDeclCommand(pf.portDecl, model)
      controller.exec(com)
      exit()
    }
    def buttonDown() {}
    def exit() { pf.hideFeedback; pf = null; selecting.enter() }
  }
}
