package org.zaluum.nide.zge

import org.zaluum.nide.compiler._
import org.zaluum.nide.zge.draw2dConversions._
import scala.annotation.tailrec
trait Vertex {
  def p: Point
  def isEnd = false
  def move(v:Vector2) : Vertex
  //override def toString = "v(" + p + ")"
}
class TempEnd(val p: Point) extends Vertex {
  override def isEnd = true
  def move(v:Vector2) = new TempEnd(p+v)
}
class Joint(val p: Point) extends Vertex {
  override def toString = "joint(" + p + " " + hashCode + ")"
  def move(v:Vector2) = new Joint(p+v)
}
class PortVertex(val port: PortFigure, val p : Point) extends Vertex {
  val portPath = port.portPath
  def toRef = PortRef(
    port.valSym.map { s ⇒ ValRef(s.name) } getOrElse { ThisRef },
    port.sym.name,
    port.in)
  override def isEnd = true
  def move(v:Vector2) = new PortVertex(port, p+v)
}
object Edge {
  def apply(a: Vertex, b: Vertex): Edge = new Edge(a, b, List(b.p, a.p),None)
  def apply(c: ConnectionDef): Edge = {
    val points = c.points
    val a = new TempEnd(points.head)
    val b = new TempEnd(points.last)
    new Edge(a, b, points,Some(c))
  }
}
class Edge(val a: Vertex, val b: Vertex, val points: List[Point], val c: Option[ConnectionDef]) {
  assert(points.size >= 2)
 // assert(a.p == points.head)
 // assert(b.p == points.last)
  override def toString = "Edge(" + a + "," + b + "," + points + "," + hashCode + ")"
  def vertexs = List(a, b)
  lazy val lines: List[Line] = makePath(points)
  def linesString = lines.mkString(",")
  private def ¬(src: Point, dst: Point) = {
    if (src == dst) {
      List()
    } else {
      if (src.x == dst.x)
        List(Line(src, dst, false))
      else if (src.y == dst.y)
        List(Line(src, dst, true))
      else
        List(Line(src, dst, true), Line(src, dst, false))
    }
  }
  private def makePath(path: List[Point]): List[Line] = {
    path match {
      case Nil ⇒ Nil
      case e :: Nil ⇒ Nil
      case from :: to :: tail ⇒ ¬(from, to) ::: makePath(to :: tail)
    }
  }
  def fixStart : Edge = new Edge(a,b, a.p :: points.tail,c)
  def fixEnd : Edge = new Edge(a,b, points.dropRight(1) :+ b.p,c)
  def isParallel(o: Edge) = { (a == o.a && b == o.b) || (a == o.b && b == o.a) }
  def intersections(e: Edge): (List[Point], List[Point]) = {
    val thisSorted = for (l ← lines; ol ← e.lines; i ← l.intersect(ol, false)) yield (i, l, ol)
    val otherSorted = e.lines flatMap { l ⇒
      thisSorted.filter { case (_, _, ol) ⇒ ol == l }.
        sortBy { case (i, _, _) ⇒ l.start.distanceOrto(i) }
    }
    (thisSorted map { case (i, _, _) ⇒ i }, otherSorted.map { case (i, _, _) ⇒ i })
  }
  def reverse = {
    val p = lines.head.start :: (for (l ← lines) yield l.end)
    new Edge(b, a, p.reverse,c)
  }
  
  def move(moveLines: Set[Line], v: Vector2) = {
    var moveH = Set[Point]()
    var moveV = Set[Point]()
    for (l ← moveLines; if (lines.contains(l))) {
      if (l.horizontal) {
        assert(points.contains(l.start))
        val prefix = points.takeWhile(p ⇒ p != l.start)
        moveH ++= prefix.reverse.takeWhile(p ⇒ p.x == l.start.x)
        val suffix = points.dropWhile(p ⇒ p != l.start).drop(1)
        moveH ++= suffix.takeWhile(p ⇒ p.x == l.end.x)
        moveV ++= prefix.reverse.takeWhile(p ⇒ p.y == l.start.y)
        moveV ++= suffix.takeWhile(p ⇒ p.y == l.end.y)
        moveV += l.start
        moveH += l.start
      } else {
        assert(points.contains(l.end))
        val prefix = points.takeWhile(p ⇒ p != l.end)
        moveH ++= prefix.reverse.takeWhile(p ⇒ p.x == l.end.x)
        val suffix = points.dropWhile(p ⇒ p != l.end).drop(1)
        moveH ++= suffix.takeWhile(p ⇒ p.x == l.end.x)
        moveV ++= prefix.reverse.takeWhile(p ⇒ p.y == l.start.y)
        moveV ++= suffix.takeWhile(p ⇒ p.y == l.end.y)
        moveH += l.end
        moveV += l.end
      }
    }
    val newPoints = for (p ← points) yield {
      if (moveH.contains(p) && moveV.contains(p)) p + v
      else if (moveH.contains(p)) Point(p.x + v.x, p.y)
      else if (moveV.contains(p)) Point(p.x, p.y + v.y)
      else p
    }
    val fixedPoints = (a.p :: newPoints.tail).dropRight(1) ::: b.p :: Nil
    new Edge(a, b, fixedPoints,c)
  }
  def splitAt(v: Vertex): (Edge, Option[Edge]) = {
    if (v.p == a.p || v.p == b.p) (this, None)
    else {
      val seg = lines.find(_.contains(v.p)).getOrElse {
        throw new RuntimeException("Cannot split " + this.linesString + " at " + v.p)
      }
      val before = points.takeWhile(_ != seg.from)
      val after = points.dropWhile(_ != seg.to).drop(1)
      val ebefore = new Edge(a, v, before ::: seg.from :: v.p :: Nil,c)
      val eafter = new Edge(v, b, v.p :: seg.to :: after,c)
      (ebefore, Some(eafter))
    }
  }
  // removes loops. convert to graph and serach shortest path
  def untangle: Edge = {
    case class VertexWP(p: Point) extends Vertex {
      def move(v:Vector2) = VertexWP(p+v)
    }
    val fakeVertexs: Set[Vertex] = points.map { VertexWP(_) }.toSet
    val fakeEdges = points.sliding(2, 1).map { pair ⇒
      val a = pair.head
      val b = pair.last
      new Edge(VertexWP(a), VertexWP(b), List(a, b),c)
    }.toSet
    var g: ConnectionGraph = new ConnectionGraphV(fakeVertexs, Set(fakeEdges.head))
    def fill(g: ConnectionGraph, edges: List[Edge]) = {
      edges.foldLeft(g)((g, e) ⇒ g.addNoParallel(e))
    }
    g = fakeEdges.tail.foldLeft(g)((g, e) ⇒ g.addMaster2(e, fill))
    val path = g.findShortestPath(VertexWP(a.p), VertexWP(b.p), Set())
    new Edge(a, b, path map { _.p },c)
  }
  def contains(p: Point) = lines.exists(_.contains(p))
  def liesIn(from: Point, mid: Point, to: Point) = mid.y == from.y || mid.x == to.x
  // merges two adjacent edges
  def merge(e: Edge): (Vertex, Edge) = { // TODO simplify route
    def mergePoints(from: List[Point], to: List[Point]) = {
      assert(from.last == to.head)
      assert(from.size >= 2)
      assert(to.size >= 2)
      val nfrom = from.dropRight(1)
      val nto = to.tail
      val mid = to.head
      if (liesIn(nfrom.last, mid, nto.head)) // Fixme direction change bug?
        nfrom ::: nto
      else
        nfrom ::: mid :: nto
    }
    if (e.a == a) { // <-=>
      (a, new Edge(e.b, b, mergePoints(e.reverse.points, points),e.c))
    } else if (e.a == b) { //   =>->
      (b, new Edge(a, e.b, mergePoints(points, e.points),c))
    } else if (e.b == a) { // ->=>
      (a, new Edge(e.a, b, mergePoints(e.points, points),e.c))
    } else if (e.b == b) { //   =><-
      (b, new Edge(a, e.a, mergePoints(points, e.reverse.points),c))
    } else throw new Exception()
  }
  def extend(to: Vertex, dir: OrtoDirection): Edge = {
    val res = points match {
      case Nil ⇒ new Edge(a, to, to.p :: Nil,c)
      case from :: Nil ⇒ new Edge(a, to, from :: to.p :: Nil,c)
      case h ⇒
        val mid = h.last
        val from = h(h.size - 2)
        // src mid
        if (dir == H) {
          if (liesIn(from, mid, to.p)) // we can suppress h if it lies in the L from hh to p
            new Edge(a, to, h.dropRight(1) :+ to.p,c)
          else // we make the longest L possible
            new Edge(a, to, h :+ to.p,c)
        } else {
          val newmid = Point(mid.x, to.p.y)
          new Edge(a, to, h.dropRight(1) :+ newmid :+ to.p,c)
        }
    }
    res
  }
}
class ConnectionGraphV(val vertexs: Set[Vertex], val edges: Set[Edge]) extends ConnectionGraph
abstract class ConnectionGraph {
  val vertexs: Set[Vertex]
  val edges: Set[Edge]
  lazy val vertexConnect: Map[Vertex, Set[Vertex]] = {
    var map = Map[Vertex, Set[Vertex]]()
    for (v ← vertexs) {
      map += (v -> Set())
    }
    for (e ← edges) {
      map += (e.a -> (map(e.a) + e.b))
      map += (e.b -> (map(e.b) + e.a))
    }
    map
  }
  lazy val components: Set[ConnectionGraph] = {
    var allComponents = vertexs map { v ⇒ v -> Set(v) } toMap;
    for (e ← edges) {
      val newSet = allComponents(e.a) ++ allComponents(e.b)
      allComponents += (e.a -> newSet)
      allComponents += (e.b -> newSet)
    }
    allComponents.values.toSet map { vs: Set[Vertex] ⇒
      new ConnectionGraph {
        val vertexs = vs
        lazy val edges = ConnectionGraph.this.edges filter { e ⇒ vs.contains(e.a) || vs.contains(e.b) }
      }
    }
  }
  lazy val hasCycle: Boolean = {
    var visited = Set[Vertex]()
    def findCycle(v: Vertex, from: Vertex): Boolean = { // turn into iterative
      if (visited.contains(v)) true
      else {
        visited += v
        vertexConnect(v).view.filter { _ != from } exists { u ⇒ findCycle(u, v) }
      }
    }
    vertexs.exists { v ⇒ !visited.contains(v) && findCycle(v, null) }
  }
  def split(e1: Edge, e2: Edge, inivertexs: Set[Vertex]): (Vector[Edge], Set[Vertex], List[Point]) = {
    var myvertexs = inivertexs
    def vertexAt(p: Point) = myvertexs find { _.p == p } getOrElse {
      val j = new Joint(p)
      myvertexs += j
      j
    }
    val (isecs1, isecs2) = e1.intersections(e2)
    var segments = Vector[Edge]()
    var remainingMe = e2
    for (i ← isecs2) {
      remainingMe.splitAt(vertexAt(i)) match {
        case (before, None) ⇒ //println("happens!")
        //assert(i==points.last)// can only happen if it's the last isec 
        case (before, Some(after)) ⇒
          remainingMe = after
          segments = segments :+ before
      }
    }
    segments = segments :+ remainingMe
    (segments, myvertexs, isecs1)
  }
  def addMaster2(master: Edge, fill: (ConnectionGraph, List[Edge]) ⇒ ConnectionGraph): ConnectionGraph = {
    var myVertexs = vertexs
    var edgesResult = List[Edge]()
    var masterPoints = Set[Point]()
    for (e ← edges) {
      val (efs, nv, mPoints) = split(master, e, myVertexs)
      masterPoints ++= mPoints
      myVertexs = nv
      edgesResult ++= efs
    }
    @tailrec // have remaining ordered
    def splitMaster(edges: Set[Edge], remaining: Set[Point]): Set[Edge] = {
      if (remaining.isEmpty) edges
      else {
        var newEdges = Set[Edge]()
        val p = remaining.head
        for (e ← edges) {
          if (e.contains(p)) {
            val (e1, e2) = e.splitAt(myVertexs.find(_.p == p).get)
            newEdges += e1
            newEdges ++= e2
          } else newEdges += e
        }
        splitMaster(newEdges, remaining.tail)
      }
    }
    val masters = splitMaster(Set(master), masterPoints)
    var g: ConnectionGraph = new ConnectionGraphV(myVertexs, masters)
    fill(g, edgesResult)
  }
  def addTree(e: Edge): ConnectionGraph = {
    if (edges.exists(_.isParallel(e))) this
    else {
      val test = new ConnectionGraphV(vertexs, edges + e)
      if (test.hasCycle) this else test
    }
  }
  def addNoParallel(e: Edge): ConnectionGraph = {
    if (edges.exists(_.isParallel(e))) this
    else new ConnectionGraphV(vertexs, edges + e)
  }
  def addMaster(e: Edge) = {
    val ca = component(e.a)
    val cb = component(e.b)
    val unaffectedEdges = edges filterNot { e ⇒ ca.edges(e) || cb.edges(e) }
    val affected = ca.edges ++ cb.edges
    def fill(g: ConnectionGraph, edges: List[Edge]) = edges.foldLeft(g)((g, e) ⇒ g.addTree(e))
    val g = new ConnectionGraphV(vertexs, affected).addMaster2(e, fill).clean.pruneTree
    new ConnectionGraphV(g.vertexs, g.edges ++ unaffectedEdges).clean
  }
  def findShortestPath(a: Vertex, b: Vertex, visited: Set[Vertex]): List[Vertex] = {
    assert(vertexs.contains(a))
    assert(vertexs.contains(b))
    if (a == b) List(b)
    else {
      val others = vertexConnect(a)
      var min = Integer.MAX_VALUE
      var candidate = List[Vertex]()
      for (o ← others; if (!visited.contains(o))) {
        val path = findShortestPath(o, b, visited + a)
        if (!path.isEmpty && path.length < min) {
          candidate = a :: path
          min = path.length
        }
      }
      candidate
    }
  }
  def moveVertexs(moveVertexs : Set[_ <: Vertex], v:Vector2): ConnectionGraph = {
    val map = moveVertexs.map { vx:Vertex => (vx->vx.move(v)) }.toMap
    val newVertexs = vertexs -- moveVertexs ++ map.values
    edges.foldLeft(this) { (current,edge) =>
      val newEdge = new Edge(map.getOrElse(edge.a,edge.a), map.getOrElse(edge.b,edge.b), edge.points,edge.c).fixStart.fixEnd
      new ConnectionGraphV(newVertexs, current.edges - edge + newEdge)
    }
  }
  def vertexAtPos(p: Point): Option[Vertex] = vertexs.find { _.p == p }
  def component(v: Vertex) = components.find { c ⇒ c.vertexs.contains(v) }.get
  def add(v: Vertex) = {
    edges.find { e ⇒ e.contains(v.p) } map { edge ⇒
      if (edge.a == v || edge.b == v) this
      else {
        val (n1, n2) = edge.splitAt(v)
        new ConnectionGraphV(vertexs + v, (edges - edge) + n1 ++ n2.toSet)
      }
    } getOrElse { new ConnectionGraphV(vertexs + v, edges) }
  }
  def remove(e: Edge) = new ConnectionGraphV(vertexs, edges - e)
  // preconditions: it's a tree and it's connected
  def pruneTree: ConnectionGraph = {
    val keep = for (e ← edges; v ← Set(e.a, e.b); if (v.isEnd)) yield v
    if (keep.isEmpty){
      this
    }else{
      val root = keep.head
      val cover = (for (k ← keep.tail; pv ← findShortestPath(root, k, Set())) yield pv).toSet
      val coverE = for (e ← edges; if (cover.contains(e.a) && cover.contains(e.b))) yield e
      new ConnectionGraphV(vertexs, coverE)
    }
  }
  def clean: ConnectionGraph = {
    var edgesToProcess = edges
    var current = edges
    var removedVertexs = Set[Vertex]()
    while (!edgesToProcess.isEmpty) {
      val e = edgesToProcess.head
      val aAdj = current filter { o ⇒ o != e && (e.a == o.a || e.a == o.b) }
      val bAdj = current filter { o ⇒ o != e && (e.b == o.a || e.b == o.b) }
      def mergeOne(other: Edge) = {
        val (v, merged) = e.merge(other)
        edgesToProcess = edgesToProcess - e - other + merged
        current = current - e - other + merged
        removedVertexs += v
      }
      if (aAdj.size == 1) mergeOne(aAdj.head)
      else if (bAdj.size == 1) mergeOne(bAdj.head)
      else {
        edgesToProcess = edgesToProcess - e
      }
    }
    val used = for (e ← current; v ← e.vertexs) yield v
    new ConnectionGraphV(used ++ ends -- removedVertexs, current)
  }
  def ends = vertexs.filter { _.isEnd }
  def toTree = {
    var map = Map[Vertex, Junction]()
    val namer = new Namer {
      def usedNames = map.values.map { _.name.str }.toSet
    }
    val junctions: List[Junction] = vertexs.toList collect {
      case v: Joint ⇒
        val j = Junction(Name(namer.freshName("j")), v.p)
        map += (v -> j)
        j
    }

    val connections: List[Tree] = edges.map { e ⇒
      def vertexRef(v: Vertex): Tree = v match {
        case p: PortVertex ⇒ p.toRef
        case v ⇒ JunctionRef(map(v).name)
      }
      ConnectionDef(vertexRef(e.a), vertexRef(e.b), e.points)
    }.toList
    (connections, junctions)
  }
  
}