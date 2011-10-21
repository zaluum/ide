package org.zaluum.nide.zge

import scala.annotation.elidable
import scala.collection.JavaConversions.setAsJavaSet
import scala.math.abs
import org.eclipse.draw2d.geometry.{ Point ⇒ EPoint }
import org.eclipse.draw2d.Cursors
import org.eclipse.draw2d.Figure
import org.eclipse.draw2d.Polyline
import FigureHelper.isOrHas
import org.zaluum.nide.compiler._
trait ConnectionsTool {
  this: TreeTool ⇒

  // CONNECT
  trait Connecting extends ToolState with LineBlinker {
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
    var painter: PreviewConnectionPainter = _
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
          case _                 ⇒ new EmptyVertex(snap)
        }
      }
    }
    def enter(initContainer: C, initFig: Hover, initPos: Point) {
      state = this
      enterSingle(initContainer)
      paintedEdge = null
      painter = new PreviewConnectionPainter(initContainer)
      viewer.feedbackLayer.add(hFig)
      viewer.feedbackLayer.add(vFig)
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
      case Some(l: LineItem)   ⇒ l.l.project(p)
      case Some(p: PortFigure) ⇒ p.anchor
      case _                   ⇒ p
    }
    def doEnter {}
    /**
     * This tries to simplify the connection searching for intersections
     */
    def endConnection() {
      val bl = initContainer.symbol
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
              case b: Block if (b == initContainer.block) ⇒
                b.copy(
                  valDefs = transformTrees(b.valDefs),
                  parameters = transformTrees(b.parameters),
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
      if (f.getParent() == viewer.feedbackLayer)
        viewer.feedbackLayer.remove(f)
    }
    override def menu {
      exit()
    }
    def exit() {
      dst foreach { _.hover = false }
      unblinkLine()
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
        painter.paintRoute(e)
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
          case _                 ⇒ None
        }
      }
      dst match {
        case Some(l: LineItem) ⇒ blinkLine(l)
        case _                 ⇒ unblinkLine()
      }
      val now = snapMouse(dst, currentMouseLocation)
      viewer.setStatusMessage(currentMouseLocation.toString + " " + absMouseLocation.toString)
      dst foreach { _.hover = true }
      if (dst.isDefined)
        viewer.setCursor(Cursors.UPARROW) else viewer.setCursor(Cursors.CROSS)
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
          val p = initContainer.translateMineToViewport_!(point(ext.b.p))
          hFig.setStart(p)
          vFig.setStart(p)
          hFig.setEnd(p)
          vFig.setEnd(p)
        case None ⇒
          paintCreatingEdge(edge)
          if (dir == H) {
            val lastp = initContainer.translateMineToViewport_!(point(last))
            hFig.setStart(lastp)
            val mid = initContainer.translateMineToViewport_!(new EPoint(now.x, last.y))
            hFig.setEnd(mid)
            vFig.setStart(mid)
            val pnow = initContainer.translateMineToViewport_!(point(now))
            vFig.setEnd(pnow)
          } else {
            val plast = initContainer.translateMineToViewport_!(point(last))
            vFig.setStart(plast)
            val mid = initContainer.translateMineToViewport_!(new EPoint(last.x, now.y))
            vFig.setEnd(mid)
            hFig.setStart(mid)
            val pnow = initContainer.translateMineToViewport_!(point(now))
            hFig.setEnd(pnow)
          }
      }

    }
    def abort() { exit() }

  }
  object connecting extends Connecting with SingleContainerAllower

  // MOVING

  trait Moving extends ToolState {
    self: DeltaMove with SingleContainer with ContainerHighlighter ⇒
    var targetContainer: ContainerItem = _
    var targetMouseLocation: Point = _
    var movables: Set[Item] = _
    var ignoredContainers: Set[ContainerItem] = _
    def enter(initDrag: Point, initContainer: C) {
      state = this
      enterMoving(initDrag)
      enterSingle(initContainer)
      targetContainer = initContainer
      movables = viewer.selectedItems.collect { case i: Item ⇒ i }
      ignoredContainers = movables.collect { case c: ContainerItem ⇒ c } flatMap { c ⇒
        c.deepChildren.collect { case c: ContainerItem ⇒ c }.toSet + c
      }
    }

    def moveWithinContainers() {
      val frags = movables.groupBy(_.container) map {
        // calculate the movement inside each container
        case (container, movables) ⇒
          val g = container.graph
          val subjects = for (m ← movables; s ← m.selectionSubject) yield s
          val valdefs = subjects collect { case v: ValDef ⇒ v }
          val portdefs = subjects collect { case p: PortDef ⇒ p }
          val lines = subjects collect { case l: LineSelectionSubject ⇒ l }
          val groups = lines.groupBy { case LineSelectionSubject(c, l) ⇒ c }.mapValues(_.map { _.l })
          var edges = g.edges
          var vertexs = g.vertexs
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
            p.ps.fromInside && valdefs.contains(pi.valSymbol.decl)
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
          val (connections, njunctions) = result.toTree;
          { e: Transformer ⇒
            {
              case b: Block if (b == container.block) ⇒
                b.copy(
                  valDefs = e.transformTrees(b.valDefs),
                  parameters = e.transformTrees(b.parameters),
                  connections = connections,
                  junctions = njunctions)
              case v: ValDef if (valdefs.contains(v)) ⇒
                v.copy(pos = snap(v.pos + delta),
                  params = e.transformTrees(v.params),
                  template = e.transformOption(v.template))
              case p: PortDef if (portdefs.contains(p)) ⇒
                p.copy(inPos = snap(p.inPos + delta))

            }: TreePF
          }
      }
      // merge the fragments
      // can be combined because they operate on different parts of the tree
      val command = EditTransformer(e ⇒ frags.map { _(e) } reduce (_ orElse _))
      controller.exec(command)
    }
    def cutAndPaste() {
      val cut = Clipboard.createFromSelection(viewer.selectedItems)
      val moved = cut.renameRelocate(targetContainer.block.sym, delta, targetContainer, false)
      val comm = Delete.deleteSelectionAndPaste(
        movables,
        viewer.graphOf,
        Some(moved),
        Some(targetContainer))
      controller.exec(comm)
    }
    def buttonUp {
      if (initContainer == targetContainer)
        moveWithinContainers()
      else
        cutAndPaste
    }

    def drag {}
    def buttonDown {}
    def exit() { selecting.enter() }
    def move() {
      targetContainer = currentIgnoring(ignoredContainers)
      targetMouseLocation = targetContainer.translateFromViewport(absMouseLocation)
      viewer.selectedItems foreach { f ⇒ f.moveFeed(snap(f.pos + delta)) }
    }
    def abort() {
      viewer.selectedItems foreach { f ⇒ f.moveFeed(f.pos) }
      exit()
    }
  }
  class MovingItem extends Moving with DeltaMove with SingleContainer with ContainerHighlighter {
    override def highlightContainer = targetContainer
  }
  val moving = new MovingItem
}