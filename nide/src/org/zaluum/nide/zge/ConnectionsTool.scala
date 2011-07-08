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
import org.zaluum.basic.LoopBox
import FigureHelper._

trait ConnectionsTool {
  this: TreeTool ⇒

  // CONNECT
  trait Connecting extends ToolState {
    self: SingleContainer ⇒
    var g: ConnectionGraph = null
    var edge: Edge = null
    var paintedEdge: Edge = null
    var last: Point = null
    var dst: Option[Hover] = None
    var src: Option[Hover] = None
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
    def vertexAt(p: Point) = g.vertexs.find(v ⇒ v.p == p)
    def vertexFor(p: Point, o: Option[Hover]) = {
      val snap = snapMouse(o, p)
      g.vertexs.find(v ⇒ v.p == snap).getOrElse {
        o match {
          case Some(l: LineItem) ⇒ new Joint(snap)
          case _ ⇒ new EmptyVertex(snap)
        }
      }
    }
    def enter(initContainer: C, initFig: Hover, initPos: Point) {
      state = this
      enterSingle(initContainer)
      paintedEdge = null
      painter = new ConnectionPainter(initContainer)
      initContainer.feedbackLayer.add(hFig)
      initContainer.feedbackLayer.add(vFig)
      hFig.setLineStyle(org.eclipse.swt.SWT.LINE_DASHDOT);
      vFig.setLineStyle(org.eclipse.swt.SWT.LINE_DASHDOT);
      dst = None
      src = Some(initFig)
      srcPos = snapMouse(src, initPos)
      last = srcPos
      center = true
      g = initContainer.graph
      edge = Edge(vertexFor(initPos, src), vertexFor(initPos, src))
      dir = H
      move()
    }
    def snapMouse(f: Option[Figure], p: Point): Point = f match {
      case Some(l: LineItem) ⇒ l.l.project(p)
      case Some(p: PortFigure) ⇒ p.anchor
      case _ ⇒ p
    }
    def doEnter {}
    /**
     * This tries to simplify the connection searching for intersections
     */
    def endConnection() {
      val bs = initContainer.boxDef.symbol.asInstanceOf[BoxTypeSymbol]
      val wp = extend.points
      if (wp.distinct.size >= 2) {
        val vend = vertexFor(wp.last, dst)
        val vstart = vertexFor(wp.head, src)
        val newEdge = new Edge(vstart, vend, wp, None).untangle
        val newGraph = g.add(vstart).add(vend).cutAndAddToGraph(newEdge).prune.clean
        val (connections, junctions) = newGraph.toTree
        controller.exec(
          new EditTransformer {
            val trans: PartialFunction[Tree, Tree] = {
              case b: BoxDef if (b == initContainer.boxDef) ⇒
                b.copy(
                  defs = transformTrees(b.defs),
                  vals = transformTrees(b.vals),
                  ports = transformTrees(b.ports),
                  connections = connections,
                  junctions = junctions)
            }
          })
      } else exit()
    }
    def extend = edge.extend(vertexFor(currentMouseLocation, dst), dir)
    def buttonUp {
      // execute model command
      if (dst.isDefined) {
        endConnection()
      } else {
        // waypoint
        edge = extend
        last = edge.points.last
        move()
      }
    }
    override def doubleClick {
      endConnection()
      selecting.filterDouble = true
    }
    def drag {}
    def buttonDown {}
    private def removeFeed(f: Figure) {
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
    def paintCreatingEdge(e: Edge) = {
      if (paintedEdge != e) {
        painter.paintCreatingRoute(e)
        paintedEdge = e
      }
    }
    def move() {
      import math.abs
      portsTrack.update()
      dst foreach { _.hover = false }
      dst = portsTrack.current orElse {
        initContainer.itemAt(point(currentMouseLocation), false) match {
          case Some(l: LineItem) ⇒ Some(l)
          case _ ⇒ None
        }
      }
      val now = snapMouse(dst, currentMouseLocation)
      viewer.setStatusMessage(currentMouseLocation.toString + " " + absMouseLocation.toString)
      dst foreach { _.hover = true }
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
      dst match {
        case Some(p) ⇒
          val ext = extend.fixEnds
          paintCreatingEdge(ext)
          val p = point(ext.b.p)
          hFig.setStart(p)
          vFig.setStart(p)
          hFig.setEnd(p)
          vFig.setEnd(p)
        case None ⇒
          paintCreatingEdge(edge)
          if (dir == H) {
            hFig.setStart(point(last))
            val mid = new EPoint(now.x, last.y)
            hFig.setEnd(mid)
            vFig.setStart(mid)
            vFig.setEnd(point(now))
          } else {
            vFig.setStart(point(last))
            val mid = new EPoint(last.x, now.y)
            vFig.setEnd(mid)
            hFig.setStart(mid)
            hFig.setEnd(point(now))
          }
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
      val g = initContainer.graph
      val subjects = for (m ← movables; s ← m.selectionSubject) yield s
      val valdefs = subjects collect { case v: ValDef ⇒ v }
      val portdefs = subjects collect { case p: PortDef ⇒ p }
      val lines = subjects collect { case l: LineSelectionSubject ⇒ l }
      val groups = lines.groupBy { case LineSelectionSubject(c, l) ⇒ c }.mapValues(_.map { _.l })
      var edges = g.edges
      var vertexs = g.vertexs
      // FIXME correctly snap line moves
      // move lines
      for ((c, lines) ← groups; e ← g.edges; if (e.srcCon == Some(c))) {
        edges = edges - e + e.move(lines, delta)
      }
      // collect moved junctions
      var movedJunctions = Set[Vertex]()
      for (e ← edges; v ← vertexs; if !v.isEnd) {
        if (e.a == v && e.points.head != e.a.p) movedJunctions += v
        if (e.b == v && e.points.last != e.b.p) movedJunctions += v
      }
      // collect moved ends
      val movedEnds = vertexs.collect { case p: PortVertex ⇒ p }.filter { p ⇒
        val pi = p.ps.pi
        if (p.ps.fromInside) 
          portdefs.contains(pi.valSymbol.tpe.decl)
        else 
          valdefs.contains(pi.valSymbol.decl)
      }
      // update edge vertexs
      for (v ← movedJunctions.view ++ movedEnds) {
        val newv = v.move(snap(v.p + delta))
        vertexs = vertexs - v + newv
        edges = for (e ← edges) yield {
          assert(!(e.a == v && e.b == v))
          if (e.a == v) new Edge(newv, e.b, e.points, e.srcCon)
          else if (e.b == v) new Edge(e.a, newv, e.points, e.srcCon)
          else e
        }
      }
      // create graph result
      // add edges fixed and untangled
      val initGraph = new ConnectionGraphV(vertexs, edges)
      var result: ConnectionGraph = new ConnectionGraphV(vertexs, Set())
      for (c ← initGraph.components) {
        var res: ConnectionGraph = new ConnectionGraphV(vertexs, Set())
        for (e ← c.edges) {
          res = res.cutAddToTree(e.fixEnds.untangle)(ConnectionGraph.fillTree)
        }
        result = new ConnectionGraphV(res.vertexs ++ result.vertexs, res.edges ++ result.edges)
      }
      result = result.prune.clean
      // done
      val (connections, junctions) = result.toTree
      val command = new EditTransformer {
        val trans: PartialFunction[Tree, Tree] = {
          case b: BoxDef if (b == initContainer.boxDef) ⇒
            b.copy(
              defs = transformTrees(b.defs),
              vals = transformTrees(b.vals),
              ports = transformTrees(b.ports),
              connections = connections,
              junctions = junctions)
          case v: ValDef if (valdefs.contains(v)) ⇒
            v.copy(pos = snap(v.pos + delta), params = transformTrees(v.params))
          case p: PortDef if (portdefs.contains(p)) ⇒
            p.copy(inPos = snap(p.inPos + delta))

        }
      }
      controller.exec(command)
    }
    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() { viewer.selectedItems foreach { f => f.moveFeed(snap(f.pos + delta)) } }
    def abort() {
      viewer.selectedItems foreach { f => f.moveFeed(f.pos) }
      exit()
    }
  }
  class MovingItem extends Moving with DeltaMove with SingleContainer with Allower
  val moving = new MovingItem
}