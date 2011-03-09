package org.zaluum.nide.zge

import org.zaluum.nide.compiler.NoSymbol
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.ToolTip
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint, Rectangle }
import org.zaluum.nide.compiler.{ _ }
import scala.collection.JavaConversions._
import org.zaluum.runtime.LoopBox

trait ConnectionsTool {
  this: TreeTool ⇒
  // CONNECT
  trait Connecting extends ToolState {
    self: SingleContainer ⇒
    var route: Route = null
    var dst: Option[Selectable] = None
    var src: Option[Selectable] = None
    var srcPos: Point = Point(0, 0)
    var painter: ConnectionPainter = _
    var dir: OrtoDirection = H
    var center = true

    val portsTrack = new PortTrack {
      override def onEnter(p: PortFigure) {
        if (p.container == initContainer) {
          super.onEnter(p)
        }
      }
    }
    def enter(initContainer: C, initFig: Selectable, initPos: Point) {
      state = this
      enterSingle(initContainer)
      painter = new ConnectionPainter(initContainer.asInstanceOf[BoxDefContainer])
      dst = None
      src = Some(initFig)
      srcPos = initPos
      center = true
      dir = H
      route = Route(List(Waypoint(srcPos, H)))
      move()
    }
    def doEnter {}
    def endConnection() {
      val bs = initContainer.boxDef.symbol.asInstanceOf[BoxTypeSymbol]
      dst match {
        case Some(l: LineFigure) ⇒
          route = route.extend(Waypoint(l.l.project(currentMouseLocation),H), l.l.dir.orto)
        case Some(p: PortFigure) ⇒
          route = route.extend(Waypoint(p.anchor,H),H)
        case _ ⇒
      }
      val waypoints = route.points
      var newJunctions = Set[Junction]()
      var newConnections = Set[ConnectionDef]()
      var delConnections = Set[ConnectionDef]()
      def createJunction(l: LineFigure, p: Point): Junction = {
        val con = l.con.get
        val splitPoint = l.l.project(p)
        val (before, after) = con.route.split(splitPoint)
        val name = Name(bs.connections.freshName("con"))
        val j = Junction(name, splitPoint)
        println("splitPoint " + splitPoint)
        val beforeCon = ConnectionDef(con.tree.a, JunctionRef(name), before.points)
        val afterCon = ConnectionDef(JunctionRef(name), con.tree.b, after.points)
        newConnections += beforeCon
        newConnections += afterCon
        delConnections += con.tree
        newJunctions += j
        j
      }
      def toRef(f: Option[Figure]): Tree = f match {
        case Some(l: LineFigure) ⇒
          val j = createJunction(l, currentMouseLocation)
          JunctionRef(j.name)
        case Some(p: PortFigure) ⇒
          PortRef(
            p.valSym.map { s ⇒ ValRef(s.name) } getOrElse { ThisRef },
            p.sym.name,
            p.in)
        case _ ⇒
          EmptyTree
      }
      if (src != dst) { // MORE checks?
        val condef = ConnectionDef(
          toRef(src),
          toRef(dst),
          waypoints)
        controller.exec(
          new EditTransformer {
            val trans: PartialFunction[Tree, Tree] = {
              case b: BoxDef if (b == initContainer.boxDef) ⇒
                BoxDef(b.name, b.superName, b.image,
                  transformTrees(b.defs),
                  transformTrees(b.vals),
                  transformTrees(b.ports),
                  transformTrees(condef :: newConnections.toList ::: b.connections.filterNot { delConnections.contains(_) }),
                  newJunctions.toList ::: transformTrees(b.junctions))
            }
          })
      } else {
        exit()
      }
    }
    def extend = dst match {
      case Some(p: PortFigure) ⇒ route.extend(Waypoint(p.anchor,H),H)
      case Some(p: LineFigure) ⇒ route.changeHead(dir).extend(Waypoint(currentMouseLocation, H),p.l.dir.orto)
      case _ ⇒ Route(Waypoint(currentMouseLocation, H) :: route.changeHead(dir).points)
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
      dst = None
      src = None
      viewer.setCursor(null)
      portsTrack.hideTip
      selecting.enter()
    }

    def move() {
      import math.abs
      portsTrack.update()
      dst foreach { _.hideFeedback() }
      initContainer.itemOrLineAt(point(currentMouseLocation)) match {
        case Some(p: PortFigure) ⇒ dst = Some(p)
        case Some(l: LineFigure) ⇒ dst = Some(l)
        case _ ⇒ dst = None
      }
      dst foreach { _.showFeedback() }
      if (dst.isDefined) 
        viewer.setCursor(Cursors.ARROW) else viewer.setCursor(Cursors.CROSS) 
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

  /****
   * move connections
   */
  trait SegmentMoving extends ToolState {
    self: SingleContainer with DeltaMove ⇒
    var lf: LineFigure = null
    var painter: ConnectionPainter = _
    var route: Route = _
    var before: List[Waypoint] = _
    var after: List[Waypoint] = _
    def enter(initPoint: Point, lf: LineFigure, initContainer: BoxDefContainer) {
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
        val p = if (lf.l.from.d == V) Waypoint(lf.l.to.x + delta.x, currentMouseLocation.y, lf.l.to.d)
        else Waypoint(currentMouseLocation.x, lf.l.to.y + delta.y, lf.l.to.d)
        Route((after.dropRight(1) :+ p) ::: before)
      }
    }
    def move() {
      println(delta)
      painter.paintRoute(newRoute, false)
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
    def abort() { exit() }
    def exit() { painter.clear; selecting.enter }
  }
  object segmentMoving extends SegmentMoving with SingleContainer with DeltaMove
}