package org.zaluum.nide.zge

import org.zaluum.nide.compiler.NoSymbol
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.ToolTip
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point => EPoint, Rectangle }
import org.zaluum.nide.compiler.{ _ }
import scala.collection.JavaConversions._
import org.zaluum.runtime.LoopBox

trait ConnectionsTool {
  this : TreeTool => 
  // CONNECT
  trait Connecting extends ToolState {
    self: SingleContainer ⇒
    var route: Route = null
    var dst: Option[PortFigure] = None
    var src: Option[PortFigure] = None
    var painter: ConnectionPainter = _
    var dir: OrtoDirection = H
    var center = true
    val portsTrack = new PortTrack {
      override def onEnter(p: PortFigure) {
        if (p.container == initContainer) {
          super.onEnter(p)
          dst = Some(p)
        }
      }
      override def onExit(p: PortFigure) { super.onExit(p); dst = None }
    }
    def enter(initContainer: C, initPort: PortFigure) {
      state = this
      enterSingle(initContainer)
      painter = new ConnectionPainter(initContainer.asInstanceOf[BoxDefContainer])
      src = Some(initPort)
      center = true
      dir = H
      route = Route(List(Waypoint(initPort.anchor, H)))
      viewer.setCursor(Cursors.HAND)
      move()
    }
    def doEnter {}
    def endConnection() {
      def dstWaypoints = dst map { dstPort ⇒
        route = route.close(dstPort.anchor)
        route.points.drop(1)
      } getOrElse (route.points)
      val waypoints = if (src.isDefined) dstWaypoints.dropRight(1) else dstWaypoints
      def toPortRef(p: PortFigure) = {
        def toRef = p.valSym.map { s ⇒ ValRef(s.name) } getOrElse { ThisRef }
        PortRef(toRef, p.sym.name, p.in)
      }
      if (src != dst) { // MORE checks?
        val condef = ConnectionDef(
          src map { toPortRef(_) } getOrElse { EmptyTree },
          dst map { toPortRef(_) } getOrElse { EmptyTree },
          waypoints)
        controller.exec(
          new EditTransformer {
            val trans: PartialFunction[Tree, Tree] = {
              case b: BoxDef if (b == initContainer.boxDef) ⇒
                BoxDef(b.name, b.superName, b.image,
                  transformTrees(b.defs),
                  transformTrees(b.vals),
                  transformTrees(b.ports),
                  condef :: transformTrees(b.connections),
                  transformTrees(b.connections))
            }
          })
      } else {
        exit()
      }
    }
    def extend = dst match {
      case Some(p) ⇒ route.close(p.anchor)
      case None ⇒ Route(Waypoint(currentMouseLocation, H) :: route.changeHead(dir).points)
    }
    def buttonUp {
      // execute model command
      if (dst.isDefined) {
        endConnection()
      } else {
        // waypoint
        route = route.changeHead(dir).extend(Waypoint(currentMouseLocation, H))
        println(route)
        move()
      }
    }
    override def doubleClick {
      endConnection()
    }
    def drag {}
    def buttonDown {}
    def exit() {
      painter.clear
      dst foreach { _.hideFeedback }
      dst = None
      viewer.setCursor(null)
      portsTrack.hideTip
      selecting.enter()
    }

    def move() {
      import math.abs
      portsTrack.update()
      val v = currentMouseLocation - route.head
      val d = abs(v.x) + abs(v.y)
      if (d < 4) center = true
      if (center) {
        if (abs(v.x) > abs(v.y)) {
          dir = H
        } else {
          dir = V
        }
        if (d > 6) center = false
      }
      painter.paintCreatingRoute(extend)
    }
    def abort() { exit() }

  }
  object connecting extends Connecting with SingleContainer
  // move connections
  trait SegmentMoving extends ToolState {
    self :  SingleContainer with DeltaMove  =>
    var lf: LineFigure = null
    var painter: ConnectionPainter = _
    var route: Route = _
    var before: List[Waypoint] = _
    var after: List[Waypoint] = _
    def enter(initPoint:Point, lf: LineFigure, initContainer: BoxDefContainer) {
      enterMoving(initPoint)
      enterSingle(initContainer)
      state = this
      println("segment moving " + delta + " " + initPoint)
      this.lf = lf
      this.route = lf.r
      val lastIndex = route.points.lastIndexOf(lf.l.from)
      val (after, before) = route.points.splitAt(lastIndex)
      this.before = before
      this.after = after
      lf.con foreach { _.hide }
      painter = new ConnectionPainter(initContainer.asInstanceOf[BoxDefContainer])
      move()
    }
    def newRoute = {
      if (lf.l.primary) {
        val p = if (lf.l.from.d == V) Waypoint(currentMouseLocation.x, lf.l.from.y + delta.y, V)
        else Waypoint(lf.l.from.x + delta.x, currentMouseLocation.y, H)
        val newBefore = before match {
          case m :: tail ⇒ p :: tail
          case Nil ⇒ Nil
        }
        Route(after ::: newBefore)
      } else {
        val p = if (lf.l.from.d == V) Waypoint(lf.l.to.x + delta.x, currentMouseLocation.y, lf.l.to.d )
        else Waypoint(currentMouseLocation.x, lf.l.to.y + delta.y, lf.l.to.d)
        Route((after.dropRight(1) :+ p) ::: before)
      }
    }
    def move() {
      println(delta)
      painter.paintRoute(newRoute,false)
    }
    def buttonUp() {
      val oldcon = lf.con.get.tree
      val newcon = oldcon.copy(wayPoints = newRoute.points)
      val b = initContainer.boxDef
      controller.exec(
        new EditTransformer {
          val trans: PartialFunction[Tree, Tree] = {
            case b: BoxDef if (b == initContainer.boxDef) ⇒
              BoxDef(b.name, b.superName, b.image,
                transformTrees(b.defs),
                transformTrees(b.vals),
                transformTrees(b.ports),
                newcon :: transformTrees(b.connections.filter { _ != oldcon }),
                transformTrees(b.junctions))
          }
        })
    }
    def buttonDown() {}
    def drag() {}
    def abort() {  exit() }
    def exit() { painter.clear; selecting.enter }
  }
  object segmentMoving extends SegmentMoving  with SingleContainer with DeltaMove 
}