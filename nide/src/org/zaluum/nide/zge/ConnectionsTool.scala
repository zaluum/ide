package org.zaluum.nide.zge

import org.eclipse.draw2d.Polyline
import org.zaluum.nide.compiler.NoSymbol
import org.eclipse.swt.SWT
import org.eclipse.swt.widgets.ToolTip
import draw2dConversions._
import org.eclipse.draw2d.{ Cursors, Figure }
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint, Rectangle }
import org.zaluum.nide.compiler.{ _ }
import scala.collection.JavaConversions._
import org.zaluum.runtime.LoopBox
import FigureHelper._

trait ConnectionsTool {
  this: TreeTool ⇒
  case class PortVertex(val port: PortFigure) extends Vertex {
    def p = port.anchor
    val portPath = port.portPath
    def toRef = PortRef(
      port.valSym.map { s ⇒ ValRef(s.name) } getOrElse { ThisRef },
      port.sym.name,
      port.in)
    override def isEnd = true
  }
  def modifyGraph(initContainer: C, newGraph: ConnectionGraph) {
    var map = Map[Vertex, Junction]()
    val namer = new Namer {
      def usedNames = map.values.map { _.name.str }.toSet
    }
    val junctions: List[Junction] = newGraph.vertexs.toList collect {
      case v: Joint ⇒
        val j = Junction(Name(namer.freshName("j")), v.p)
        map += (v -> j)
        j
    }

    val connections: List[Tree] = newGraph.edges.map { e ⇒
      def vertexRef(v: Vertex): Tree = v match {
        case p: PortVertex ⇒ p.toRef
        case v ⇒ JunctionRef(map(v).name)
      }
      ConnectionDef(vertexRef(e.a), vertexRef(e.b), e.points)
    }.toList
    controller.exec(
      new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if (b == initContainer.boxDef) ⇒
            BoxDef(b.name, b.superName, b.image,
              transformTrees(b.defs),
              transformTrees(b.vals),
              transformTrees(b.ports),
              connections,
              junctions)
        }
      })
  }
  def createGraph(initContainer: C)  : (ConnectionGraph,Map[ConnectionDef,Edge])= {
    val ports = initContainer.portsLayer.getChildren collect { case port: PortFigure ⇒ PortVertex(port) }
    val junctions = initContainer.boxDef.junctions.collect { case j: Junction ⇒ (j -> new Joint(j.p)) }.toMap
    val edges = initContainer.boxDef.connections.map {
      case c: ConnectionDef ⇒
        def toVertex(t: Tree, start: Boolean): Vertex = t match {
          case JunctionRef(name) ⇒ junctions.collect { case (k, joint) if (k.name == name) ⇒ joint }.head
          case p: PortRef ⇒ ports.find { _.portPath == PortPath(p) }.get
        }
        (c -> new Edge(toVertex(c.a, true), toVertex(c.b, false), c.points))
    }.toMap
    (new ConnectionGraphV(ports.toSet ++ junctions.values, edges.values.toSet), edges)
  }
  // CONNECT
  trait Connecting extends ToolState {
    self: SingleContainer ⇒
    var g: ConnectionGraph = null
    var edge: Edge = null
    var last:Point = null
    var dst: Option[Item] = None
    var src: Option[Item] = None
    var hFig = new Polyline()
    val vFig = new Polyline()
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
    def vertexAt(p: Point) = g.vertexs.find(v ⇒ v.p == p) getOrElse (new Joint(p))

    def enter(initContainer: C, initFig: Item, initPos: Point) {
      state = this
      enterSingle(initContainer)
      painter = new ConnectionPainter(initContainer.asInstanceOf[BoxDefContainer])
      initContainer.feedbackLayer.add(hFig)
      initContainer.feedbackLayer.add(vFig)
      hFig.setLineStyle(org.eclipse.swt.SWT.LINE_DASHDOT);
      vFig.setLineStyle(org.eclipse.swt.SWT.LINE_DASHDOT);
      dst = None
      src = Some(initFig)
      srcPos = snapMouse(src, initPos)
      last = srcPos
      center = true
      val (graph,_) = createGraph(initContainer)
      g = graph
      edge = Edge(vertexAt(srcPos), vertexAt(srcPos))
      dir = H
      move()
    }
    def snapMouse(f: Option[Figure], p: Point): Point = f match {
      case Some(l: LineFigure) ⇒ l.l.project(p)
      case Some(p: PortFigure) ⇒ p.anchor
      case _ ⇒ p
    }
    def doEnter {}
    /**
     * This tries to simplify the connection by searching for intersections
     */
    def endConnection() {
      val bs = initContainer.boxDef.symbol.asInstanceOf[BoxTypeSymbol]
      val wp = extend.points
      val vend = vertexAt(wp.last)
      val vstart = vertexAt(wp.head)
      val newEdge = new Edge(vstart, vend, wp).untangle
      println("endConnection newEdge=" + newEdge.linesString)
      val newGraph = g.add(vstart).add(vend).addMaster(newEdge)
      println(newGraph.vertexs)
      modifyGraph(initContainer, newGraph)
    }
    def extend = edge.extend(vertexAt(snapMouse(dst, currentMouseLocation)),dir)
    def buttonUp {
      // execute model command
      if (dst.isDefined) {
        endConnection()
      } else {
        // waypoint
        edge = extend
        last = edge.points.last
        painter.paintCreatingRoute(edge)
        println(edge)
        move()
      }
    }
    override def doubleClick {
      endConnection()
    }
    def drag {}
    def buttonDown {}
    private def removeFeed(f:Figure) {
      if (initContainer.feedbackLayer.getChildren.contains(f))
        initContainer.feedbackLayer.remove(f)
    }
    def exit() {
      painter.clear
      removeFeed(hFig)
      removeFeed(vFig)
      dst = None
      src = None
      viewer.setCursor(null)
      portsTrack.hideTip
      selecting.enter()
    }

    def move() {
      import math.abs
      val now = snapMouse(dst, currentMouseLocation)
      portsTrack.update()
      dst foreach { _.hideFeedback() }
      viewer.setStatusMessage(currentMouseLocation.toString)
      initContainer.itemAt(point(currentMouseLocation)) match {
        case Some(p: PortFigure) ⇒ dst = Some(p)
        case Some(l: LineFigure) ⇒ dst = Some(l)
        case _ ⇒ dst = None
      }
      dst foreach { _.showFeedback() }
      if (dst.isDefined)
        viewer.setCursor(Cursors.ARROW) else viewer.setCursor(Cursors.CROSS)
      val v = now - last
      val d = abs(v.x) + abs(v.y)
      if (d < 8) center = true
      if (center) {
        if (abs(v.x) > abs(v.y)) {
          dir = H
        } else {
          dir = V
        }
        if (d > 10) center = false
      }
      
      if (dir == H) {
        hFig.setStart(point(last))
        val mid = new EPoint(now.x, last.y) 
        hFig.setEnd(mid)
        vFig.setStart(mid)
        vFig.setEnd(point(now))
      }else {
        vFig.setStart(point(last))
        val mid = new EPoint(last.x, now.y)
        vFig.setEnd(mid)
        hFig.setStart(mid)
        hFig.setEnd(point(now))
      }
    }
    def abort() { exit() }

  }
  object connecting extends Connecting with SingleContainer

  // MOVING

  trait Moving extends ToolState {
    self: DeltaMove with SingleContainer ⇒
    def enter(initDrag: Point, initContainer: C) {
      enterMoving(initDrag)
      enterSingle(initContainer)
      state = this
    }
    def allowed = (current eq initContainer) || (movables.exists { isOrHas(_, current) })
    def movables = viewer.selectedItems.collect {
      case item: Item if item.container == initContainer ⇒ item
    }
    def buttonUp {
      val (g,edges) = createGraph(initContainer)
      val subjects = for (m ← movables; s ← m.selectionSubject) yield s
      val lines = subjects collect { case l: LineSelectionSubject ⇒ l }
      val groups = lines.groupBy { case LineSelectionSubject(c, l) ⇒ c }.mapValues(_.map { _.l })      
      val edgeMap = for ((c, lines) ← groups; e ← edges.get(c)) yield {
        (e, e.move(lines, delta).untangle)
      }
      println("edgeMap = " + edgeMap)
      var result: ConnectionGraph = new ConnectionGraphV(g.vertexs, g.edges -- edgeMap.keys)
      for ((_, newe) ← edgeMap) { result = result.addMaster(newe) }
      modifyGraph(initContainer, result)

      /*val positions = movables.collect { case item:TreeItem ⇒
        val oldLoc = item.getBounds.getLocation
        (item.tree.asInstanceOf[Tree] -> (Point(oldLoc.x, oldLoc.y) + delta))
      }.toMap
      
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) if (positions.contains(v)) ⇒
            ValDef(name, typeName, positions(v), size, guiPos, guiSize,params)
          case p: PortDef if (positions.contains(p)) ⇒
            p.copy(inPos = positions(p))
        }
      }
      controller.exec(command)*/
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { viewer.selectedItems foreach { _.moveDeltaFeed(delta) } }
    def abort() {
      viewer.selectedItems foreach { _.moveDeltaFeed(Vector2(0, 0)) }
      exit()
    }
  }
  class MovingItem extends Moving with DeltaMove with SingleContainer with Allower
  val moving = new MovingItem
}