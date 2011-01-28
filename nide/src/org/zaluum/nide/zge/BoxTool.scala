package org.zaluum.nide.zge
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point, Rectangle }
import org.zaluum.nide.model.{ Point ⇒ MPoint, _ }
import scala.collection.JavaConversions._

class BoxTool(val viewer:Viewer) extends AbstractTool(viewer) {
  def model =viewer.model
  override def modelView = viewer.modelView
  override lazy val selecting  = new  Selecting { 
    override def connect(port : PortFigure ) {
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
    var bf: BoxFigure = _
    var boxClassDecl : BoxClassDecl = _
    def enter() {
      state = this
      val name = model.nextFreeName("C");
      println("next name " + name)
      boxClassDecl = new BoxClassDecl(
          InnerBoxClassName(model.className,name),
              imageName= None,visual=false,guiSize=Dimension(10,10))
      
      val box = new Box(
          boxClassName = boxClassDecl.className,  
          name = model.nextFreeName("box"),
          pos = MPoint(1,1),
          guiPos = None // FIXME ? 
          )
      bf = new ImageBoxFigure(box, None, modelView)
      bf.update()
      bf.hide()
      bf.showFeedback()
    }
    def move() { bf.moveFeed(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      bf.box.pos = MPoint(mouseLocation.x, mouseLocation.y)
      val com = new Command {
        val decl =boxClassDecl
        val box = bf.box
        def undo() {
          model.boxes -= box
          model.innerClassDecls -=decl
        }
        def redo() { 
          model.boxes += box
          model.innerClassDecls += decl
        }
        def canExecute = true
      }
      controller.exec(com) // no need to exit. controller aborts all tools
    }
    def buttonDown() {}
    def exit() { 
      bf.hideFeedback; 
      bf = null; 
      selecting.enter() 
    }
  }
  // CREATING BOX 
  object creating extends ToolState {
    var bf: BoxFigure = _
    def enter(boxClass: BoxClass) {
      state = this
      val box = new Box(
          boxClassName = boxClass.className,  
          name = model.nextFreeName("box"),
          pos = MPoint(1,1),
          guiPos = None // FIXME ? 
          )
      bf = new ImageBoxFigure(box, Some(boxClass), modelView)
      bf.update()
      bf.hide()
      bf.showFeedback()
    }
    def move() { bf.moveFeed(mouseLocation) }
    def abort() { exit() }
    def drag() {}
    def buttonUp() {
      bf.box.pos = MPoint(mouseLocation.x, mouseLocation.y)
      val com = new CreateCommand(bf.box, model)
      controller.exec(com) // no need to exit. controller aborts all tools
    }
    def buttonDown() {}
    def exit() { 
      bf.hideFeedback; 
      bf = null; 
      selecting.enter() 
    }
  }
  // CREATING PORT
  object creatingPort extends ToolState {
    var pf: PortDeclFigure = _
    def enter(in: Boolean) {
      state = this
      val name = model.nextFreeName("port")
      val portDecl = new PortDecl(model, name, in, "D")
      portDecl.pos = MPoint(1, 1)
      pf = new PortDeclFigure(portDecl, modelView)
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
    }
    def buttonDown() {}
    def exit() { pf.hideFeedback; pf = null; selecting.enter() }
  }
  // CONNECT
  object connecting extends MovingState {
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
      val command = new ConnectCommand(con.get, modelView.model)
      controller.exec(command)
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
}

