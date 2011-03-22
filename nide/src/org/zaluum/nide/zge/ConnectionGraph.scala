package org.zaluum.nide.zge

import org.zaluum.nide.compiler.Point
import scala.annotation.tailrec
trait Vertex {
  def p: Point
  //override def toString = "v(" + p + ")"
}
class End(val p: Point) extends Vertex {
  override def toString = "end(" + p + ")"
}
class Joint(val p: Point) extends Vertex {
  override def toString = "joint(" + p + " " + hashCode + ")"
}
object Edge {
  def apply(a: Vertex, b: Vertex): Edge = new Edge(a, b, List(b.p, a.p))
}
case class Edge(val a: Vertex, val b: Vertex, val points: List[Point]) {
  assert(points.size >= 2)
  assert(a.p == points.head)
  assert(b.p == points.last)
  override def toString = "Edge(" + a + "," + b + "," + points + "," + hashCode + ")"
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
  def isParallel(o: Edge) = { (a == o.a && b == o.b) || (a == o.b && b == o.a) }
  def intersections(e: Edge): (List[Point], List[Point]) = {
    val thisSorted = for (l ← lines; ol ← e.lines; i ← l.intersect(ol, false)) yield (i, l, ol)
    //println(thisSorted.mkString("\n\t"))
    val otherSorted = e.lines flatMap { l ⇒
      thisSorted.filter { case (_, _, ol) ⇒ ol == l }.
        sortBy { case (i, _, _) ⇒ l.start.distanceOrto(i) }
    }
    (thisSorted map { case (i, _, _) ⇒ i }, otherSorted.map { case (i, _, _) ⇒ i })
  }

  def splitAt(v: Vertex): (Edge, Option[Edge]) = {
    if (v.p == a.p || v.p == b.p) (this, None)
    else {
      val seg = lines.find(_.contains(v.p)).getOrElse {
        throw new RuntimeException("Cannot split " + this.linesString + " at " + v.p)
      }
      val before = points.takeWhile(_ != seg.from)
      val after = points.dropWhile(_ != seg.to).drop(1)
      val ebefore = new Edge(a, v, before ::: seg.from :: v.p :: Nil)
      val eafter = new Edge(v, b, v.p :: seg.to :: after)
      (ebefore, Some(eafter))
    }
  }
  // removes loops. convert to graph and serach shortest path
  def untangle: Edge = {
    case class VertexWP(p: Point) extends Vertex
    val fakeVertexs: Set[Vertex] = points.map { VertexWP(_) }.toSet
    val fakeEdges = points.sliding(2, 1).map { pair ⇒
      val a = pair.head
      val b = pair.last
      new Edge(VertexWP(a), VertexWP(b), List(a, b))
    }.toSet
    var g: ConnectionGraph = new ConnectionGraphV(fakeVertexs, Set(fakeEdges.head))
    def fill(g: ConnectionGraph, edges: List[Edge]) = {
      edges.foldLeft(g)((g, e) ⇒ g.addNoParallel(e))
    }
    g = fakeEdges.tail.foldLeft(g)((g, e) ⇒ g.addMaster2(e, fill))
    val path = g.findShortestPath(VertexWP(a.p), VertexWP(b.p), Set())
    new Edge(a, b, path map { _.p })
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
      (a, new Edge(e.b, b, mergePoints(e.points.reverse, points)))
    } else if (e.a == b) { //   =>->
      (b, new Edge(a, e.b, mergePoints(points, e.points)))
    } else if (e.b == a) { // ->=>
      (a, new Edge(e.a, b, mergePoints(e.points, points)))
    } else if (e.b == b) { //   =><-
      (b, new Edge(a, e.a, mergePoints(points, e.points.reverse)))
    } else throw new Exception()
  }
  def extend(to: Vertex): Edge = {
    val res = points match {
      case Nil ⇒ new Edge(a, to, to.p :: Nil)
      case from :: Nil ⇒ new Edge(a, to, from :: to.p :: Nil)
      case h ⇒
        val mid = h.last
        val from = h(h.size - 2)
        // src mid
        if (liesIn(from, mid, to.p)) { // we can suppress h if it lies in the L from hh to p
          // src to
          new Edge(a, to, h.dropRight(1) :+ to.p)
        } else { // we make the longest L possible 
          /*(from.d, mid.d) match {
            case (V, H) ⇒
              Route(to :: Waypoint(to.x, mid.y, V) :: from :: tail)
            case (H, V) ⇒
              Route(to :: Waypoint(mid.x, to.y, H) :: from :: tail)
            case _ ⇒*/
          new Edge(a, to, h :+ to.p)
          //}
        }
    }
    //println ("extended to " + res)
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
    println("e1 = " + e1.linesString)
    println("e2 = " + e2.linesString)
    val (isecs1, isecs2) = e1.intersections(e2)
    println("isecs2 = " + isecs2)
    println("isecs1 = " + isecs1)
    var segments = Vector[Edge]()
    var remainingMe = e2
    for (i ← isecs2) {
      remainingMe.splitAt(vertexAt(i)) match {
        case (before, None) ⇒ //println("happens!")
        //assert(i==points.last)// can only happen if it's the last isec 
        case (before, Some(after)) ⇒
          println("splited " + remainingMe.linesString + " at " + i )
          println("before " + before.linesString)
          println("after " + after.linesString)
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
    println("adding master edge = " + master.linesString )
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
    val g = new ConnectionGraphV(vertexs, affected).addMaster2(e, fill).clean
    new ConnectionGraphV(g.vertexs, g.edges ++ unaffectedEdges)
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
    new ConnectionGraphV(vertexs -- removedVertexs, current)
  }

}