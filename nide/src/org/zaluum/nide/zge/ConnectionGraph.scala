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
class Joint(val p: Point) extends Vertex{
  override def toString = "joint(" + p +" " +hashCode+ ")"
}
object Edge {
  def apply(a: Vertex, b: Vertex): Edge = new Edge(a, b, List(b.p, a.p))
}
case class Edge(val a: Vertex, val b: Vertex, val points: List[Point]) {
  assert(points.size >= 2)
  assert(a.p == points.last)
  assert(b.p == points.head)
  override def toString = "Edge("+a+","+b+","+points+"," + hashCode + ")"
  lazy val lines: List[Line] = makePath(points)
  private def ¬(src: Point, dst: Point) = {
    if (src == dst) {
      List()
    } else {
      val despl = dst.x - src.x
      List(Line(src, dst, true), Line(src, dst, false))
    }
  }
  private def makePath(path: List[Point]): List[Line] = {
    path match {
      case Nil ⇒ Nil
      case e :: Nil ⇒ Nil
      case to :: from :: tail ⇒ ¬(from, to) ++ makePath(from :: tail)
    }
  }
  def isParallel (o:Edge) = { (a==o.a && b==o.b) || (a==o.b && b==o.a)}
  def firstIntersection(e: Edge,veto:Option[Point]): Option[Point] = {
    @tailrec
    def intersection(remaining: List[Line]): Option[Point] = {
      remaining match {
        case Nil ⇒ None
        case h :: tail ⇒
          val others = e.lines
          val intersections: List[Point] = for {
            ol ← others
            p ← h.intersect(ol, false)
            if ((veto.isDefined && veto.get!=p) || veto.isEmpty) 
          } yield p
          if (intersections.isEmpty) {
            intersection(tail)
          } else {
            val p = intersections.reduceLeft { (ap, bp) ⇒
              if (ap.distanceOrto(h.from) < bp.distanceOrto(h.from)) ap else bp
            }
            Some(p)
          }
      }
    }
    intersection(lines.reverse)
  }
  def splitAt(v: Vertex): (Edge, Option[Edge]) = {
    if (v.p == a.p || v.p == b.p) (this, None)
    else {
      val seg = lines.find(_.contains(v.p)).get
      val after = points.takeWhile(_ != seg.to)
      val before = points.dropWhile(_ != seg.from).drop(1)
      val eafter = new Edge(v, b, after ::: seg.to :: v.p :: Nil)
      val ebefore = new Edge(a, v, v.p :: seg.from :: before)
      //println("split " + this + " => " + eafter + " " + ebefore)
      (ebefore, Some(eafter))
    }
  }
  def contains(p:Point) = lines.exists(_.contains(p))
  def liesIn(from: Point, mid: Point, to: Point) = {
    mid.y == from.y || mid.x == to.x
  }
  def merge(e:Edge) : (Vertex,Edge)= { // TODO simplify route
    def mergePoints(to:List[Point], from:List[Point]) = { 
      assert(to.last == from.head)
      to ::: from.tail 
    }
    if (e.a==a) { // <-=>
      (a, new Edge(e.b,b  , mergePoints(points, e.points.reverse)))
    }else if (e.a==b) { //   =>->
      (b, new Edge(a,  e.b, mergePoints(e.points, points)))
    }else if (e.b==a) {// ->=>
      (a, new Edge(e.a,b  , mergePoints(points, e.points)))
    }else if (e.b==b) { //   =><-
      (b, new Edge(a  ,e.a, mergePoints(e.points.reverse, points)))
    }else throw new Exception()
  }
  def extend(to: Vertex): Edge = {
    val res = points match {
      case Nil ⇒ new Edge(a, to, to.p :: Nil)
      case from :: Nil ⇒ new Edge(a, to, to.p :: from :: Nil)
      case mid :: from :: tail ⇒
        if (liesIn(from, mid, to.p)) { // we can suppress h if it lies in the L from hh to p 
          new Edge(a, to, to.p :: from :: tail)
        } else { // we make the longest L possible 
          /*(from.d, mid.d) match {
            case (V, H) ⇒
              Route(to :: Waypoint(to.x, mid.y, V) :: from :: tail)
            case (H, V) ⇒
              Route(to :: Waypoint(mid.x, to.y, H) :: from :: tail)
            case _ ⇒*/
          new Edge(a, to, to.p :: mid :: from :: tail)
          //}
        }
    }
    //println ("extended to " + res)
    res
  }
}
case class CutResult(me: Set[Edge], other: Set[Edge], newVertex: Set[Vertex])
class ConnectionGraphV(val vertexs: Set[Vertex], val edges: Set[Edge]) extends ConnectionGraph
object Timer {
  var start:Long = 0L
  var end:Long = 0L
  def go = {
    start = System.currentTimeMillis
  }
  def stop = {
    end = System.currentTimeMillis
    println(">   " + (end - start)/ 1000.0 + " s")
  }
}
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
  def vertexAtPos(p: Point): Option[Vertex] = vertexs.find { _.p == p }
  def component(v: Vertex) = components.find { c ⇒ c.vertexs.contains(v) }.get
  def add(v: Vertex) = {
    val e = edges.find { e => e.contains(v.p) }
    e match {
      case Some(edge) => 
        if (edge.a==v || edge.b==v)
          this
        else { 
          val (n1,n2) = edge.splitAt(v)
          new ConnectionGraphV(vertexs + v,(edges - edge)+ n1 ++ n2.toSet)
        }
      case None => 
        new ConnectionGraphV(vertexs +v, edges)
    }
  }
  def remove(e: Edge) = new ConnectionGraphV(vertexs, edges - e)
  def cut(othersp: Set[Edge]): CutResult = {
    var myvertexs = vertexs
    var edges = this.edges
    var others = othersp
    var remainingEdges = edges
    def vertexAt(p: Point) = myvertexs find { _.p == p } getOrElse {
      val j = new Joint(p)
      myvertexs += j
      j
    }
    def cutSingle(a: Edge, b: Edge, veto:Option[Point]): CutResult = { // not tail rec 
      a.firstIntersection(b,veto) match {  
        case None ⇒ CutResult(Set(a), Set(b), Set())
        case Some(p: Point) ⇒
          val v = vertexAt(p)
          val (a1, a2) = a.splitAt(v) // no more cuts possible with a1
          val (b1, b2) = b.splitAt(v)
          (a2, b2) match {
            case (Some(nextA), Some(nextB)) ⇒
              val c1 = cutSingle(nextA, nextB,Some(p))
              val c2 = cutSingle(nextA, b1,Some(p))
              CutResult(c1.me ++ c2.me + a1, c1.other ++ c2.other, c1.newVertex ++ c2.newVertex + v)
            case (Some(nextA), None) ⇒
              val c1 = cutSingle(nextA, b1,Some(p))
              CutResult(c1.me + a1, c1.other, c1.newVertex + v)
            case (None, _) ⇒ CutResult(Set(a1), Set(b1), Set(v))
          }
      }
    }
    while (!remainingEdges.isEmpty) {
      val e = remainingEdges.head
      var remainingOthers = others
      while (!remainingOthers.isEmpty) {
        val other = remainingOthers.head
        val CutResult(newme, newothers, newv) = cutSingle(e, other, None)
        others = (others - other) ++ newothers
        remainingOthers = (remainingOthers ++ newothers) - other
        edges = (edges -e ) ++ newme 
        remainingEdges = (remainingEdges ++ newme) - e
      }
    }
    CutResult(edges, others, myvertexs)
  }
  def clean: ConnectionGraph = {
    var edgesToProcess = edges
    var current = edges
    var removedVertexs = Set[Vertex]()
    while (!edgesToProcess.isEmpty) {
      val e = edgesToProcess.head
      val aAdj = current filter { o => o!=e && (e.a == o.a || e.a == o.b)}
      val bAdj = current filter { o => o!=e && (e.b == o.a || e.b == o.b)}
      def mergeOne(other:Edge) = {
        val (v,merged) = e.merge(other)
        edgesToProcess = edgesToProcess -e - other + merged
        current = current - e - other + merged 
        removedVertexs += v
      }
      if (aAdj.size ==1) mergeOne(aAdj.head) 
      else if (bAdj.size==1) mergeOne(bAdj.head)
      else {
        edgesToProcess = edgesToProcess - e
      }
    }
    new ConnectionGraphV(vertexs -- removedVertexs, current)
  }
  def add(e: Edge): ConnectionGraph = {
    assert(vertexs.contains(e.a))
    assert(vertexs.contains(e.b))
    //println("adding edge " + e + " to " + this.edges)
    val ca = component(e.a)
    val cb = component(e.b)
    val unaffectedEdges = edges filterNot { e ⇒ ca.edges(e) || cb.edges(e) }
    // add edges by cutting them all with the new edge
    // adding only the ones that do not introduce a cycle
    var remainingAffected = ca.edges ++ cb.edges
    var currentAffected = Set[Edge]()
    var currentVertexs = vertexs 
    var eFrags = Set(e)
    while (!remainingAffected.isEmpty) {
        val affected = remainingAffected.head
        val CutResult(neweFrags,affectedCut, newv) = new ConnectionGraphV(currentVertexs, eFrags).cut(Set(affected))
        currentVertexs = currentVertexs ++ newv
        currentAffected = (currentAffected - affected) ++ affectedCut        
        remainingAffected = (remainingAffected ++ affectedCut) - affected
        eFrags = neweFrags
    }
    var currentG = new ConnectionGraphV(currentVertexs, eFrags)
    for (a <- currentAffected) {
      if (!currentG.edges.exists {_.isParallel(a)}){
        val tmp  = new ConnectionGraphV(currentVertexs, currentG.edges + a)
        if (!tmp.hasCycle) currentG = tmp
      }
    }
    val res = new ConnectionGraphV(currentG.vertexs, currentG.edges ++ unaffectedEdges).clean
    //println ("result has cycle=" + res.hasCycle + " edges= " + res.edges.size + " " + res.edges )
    res
  }
}