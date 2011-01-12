package org.zaluum.nide.zge

import org.eclipse.draw2d.ColorConstants
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.{ IFigure, RectangleFigure, Cursors, Polyline }
import org.eclipse.draw2d.geometry.{ Point, Rectangle, Dimension }
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Cursor
import scala.collection.JavaConversions._
import org.zaluum.nide.model._ 


class MoveTool(viewer: Viewer) extends Tool(viewer) {
  state = selecting
  // SELECTING
  object selecting extends ToolState {
    
    var selected: Option[BoxFigure] = None
    var lineSelected : Option[LineFigure] = None
    var handle: Option[HandleRectangle] = None
    var port: Option[PortFigure] = None
    var initDrag : Point = _
    
    def buttonDown {
      selected = figureUnderMouse
      if (selected.isEmpty) lineSelected = lineUnderMouse
      initDrag = mouseLocation.getCopy
    }

    def buttonUp {
      (selected, lineSelected) match {
        case (Some(box),_) => modelView.selectedBoxes.updateSelection(Set(box),shift)
        case (None, Some(line)) => modelView.selectedLines.updateSelection(Set(line),shift)
        case (None,None) => modelView.deselectAll()
      }
    }
    val handleTrack = new OverTrack[HandleRectangle](viewer.feedbackLayer) {
      def onEnter(h:HandleRectangle) { 
        handle = Some(h)
        h.setXOR(true); 
        viewer.setCursor(h.resizeCursor) 
      }
      def onExit(f:HandleRectangle) {
        handle = None
        f.setXOR(false); 
        viewer.setCursor(null)
      }
    }
    val portsTrack = new OverTrack[PortFigure](viewer.portsLayer) {
      def onEnter(p:PortFigure) { port = Some(p); p.showFeedback }
      def onExit(p:PortFigure) { port = None; p.hideFeedback }
    }
    def move { 
      handleTrack.update()  
      portsTrack.update()
    }
    def drag {
      (handle,selected,port) match {
        case (Some(h),_,_) => // resize
          resizing.enter(initDrag,h)
        case (None, _, Some(port)) => // connect
          connect.enter(initDrag,port)
        case (None, Some(fig),_) => // select and move
          if (!modelView.selectedBoxes(fig))
            modelView.selectedBoxes.updateSelection(Set(fig),shift)
          moving.enter(initDrag)
        case (None,None,None) => marqueeing.enter(initDrag) // marquee
      }
    }
    def abort {}
    def exit {}
  }
  object connect extends MovingState {
    implicit def toP(p:Point) = P(p.x,p.y)
    var dst : Option[PortFigure] = None
    var initPort : Option[PortFigure] = None
    var con : Option[Connection] =  None
    var conf : Option[ConnectionFigure] = None
    val portsTrack = new OverTrack[PortFigure](viewer.portsLayer) {
      def onEnter(p:PortFigure) { 
        dst= Some(p); 
        con.get.to = Some(p.portRef); 
        p.showFeedback 
      }
      def onExit(p:PortFigure) { dst= None; con.get.to = None; p.hideFeedback }
    }
    def enter(initdrag:Point, initPort:PortFigure) {
      super.enter(initdrag)
      this.initPort = Some(initPort)
      con = Some(new Connection(Some(initPort.portRef),None))
      conf = Some(new ConnectionFigure(con.get,modelView))
      con.get.simpleConnect(initPort.getBounds.getCenter, mouseLocation)
      conf.get.show
      conf.get.update
      viewer.setCursor(Cursors.HAND)
    }
    def doEnter{}
    def buttonUp {
      // execute model command
      val command = new ConnectCommand(modelView.model,con.get)
      if (command.canExecute)
        controller.exec(command)
      exit()
    }
    def drag{}
    def buttonDown{}
    def exit() {
      dst foreach {_.hideFeedback}
      conf.foreach { _.hide }
      con = None
      conf = None
      viewer.setCursor(null)
      selecting.enter()
    }
    def doMove() {
      val end = dst match {
        case Some(df) => df.getBounds.getCenter
        case None => mouseLocation
      }
      con.get.simpleConnect(initPort.get.getBounds.getCenter, end)
      conf.get.update
      portsTrack.update() 
    }
    def abort() { exit() }  
  }
  // MOVE
  object moving extends MovingState {
    def doEnter{}
    def buttonUp {
      val commands= modelView.selectedBoxes.selected map {
          bf =>
          val oldLoc = bf.getBounds.getLocation
          new MoveCommand(bf.box, (oldLoc.x + d._1, oldLoc.y + d._2))
      };
      controller.exec(new ChainCommand(commands.toList))
      exit()
    }
    def drag{}
    def buttonDown{}
    def exit() { selecting.enter() }
    def doMove() {  modelView.selectedBoxes.selected foreach { _.moveDeltaFeed(d) } }
    def abort() {
      modelView.selectedBoxes.selected foreach { _.moveDeltaFeed((0,0)) }
      exit()
    }
  }
  /// MARQUEE
  object marqueeing extends MovingState {
    def doEnter(){
      viewer.setCursor(Cursors.CROSS)
      viewer.showMarquee()
      move()
    }
    def exit(){ 
      viewer.setCursor(null)
      selecting.enter() 
    }
    def abort(){ exit()}
    def buttonDown{}
    def drag{}
    def buttonUp {
      viewer.hideMarquee()
      exit()
    }
    def doMove { viewer.moveMarquee(new Rectangle(mouseLocation, initDrag)) }
  }
  // RESIZING
  object resizing extends MovingState {
    var handle : HandleRectangle = _
    def bf = handle.boxFigure
    
    def enter(initDrag: Point, handle:HandleRectangle) {
      super.enter(initDrag)
      this.handle = handle
    }
    def doEnter{}
    def buttonUp {
      val newBounds = handle.deltaAdd(d,bf.getBounds);
      //val comm = new ResizeCommand(bf.box, (newBounds.x,newBounds.y), (newBounds.width,newBounds.height))
      //controller.exec(comm)
      exit()
    }
    def doMove() {  bf.resizeDeltaFeed(delta,handle)  }
    def abort() {
      bf.resizeDeltaFeed((0,0),handle)
      exit()
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter()  }
  }
  
}
