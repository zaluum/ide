package org.zaluum.nide.zge

import org.zaluum.nide.compiler.Point
import junit.framework._
import org.junit.Assert._
import org.junit.Test
class ConnectionGraphTest { 
  /*val a = new End(Point(0, 0))
  val b1 = new End(Point(10, 10))
  val b2 = new End(Point(10, 20))
  val c = new End(Point(20, 10))
  val e1 = Edge(a, b1)
  val e2 = Edge(a, b2)
  val e3 = Edge(b1, c)

  @Test
  def lineTest() = {
    val a = new End(Point(0, 0))
    val b = new End(Point(5, 5))
    val e = Edge(a, b)
    assert(List(Line(Point(0, 0), Point(5, 5), true), Line(Point(0, 0), Point(5, 5), false)) == e.lines)
  }
  @Test
  def cycleTest() = {
    val vertexs: Set[Vertex] = Set(a, b1, b2, c)
    val edges = Set(e1, e2, e3)
    val g = new ConnectionGraphV(vertexs, edges)
    assert(!g.hasCycle)
    val g1 = new ConnectionGraphV(vertexs, edges + Edge(b1, b2))
    assert(g1.hasCycle)
  }
  @Test
  def intersectLineTest() = {
    val l1 = Line(Point(0, 0), Point(5, 10), false)
    val l2 = Line(Point(10, 5), Point(5, 8), false)
    val res = l1.intersect(l2, false)
    assert(res == List(Point(5, 5)))
    val res2 = l1.intersect(l2, true)
    assert(res2 == List(Point(5, 8)))
  }
  val x1 = new End(Point(0, 0))
  val x3 = new End(Point(15, 8))
  val y1 = new End(Point(0, 10))
  val y3 = new End(Point(5, 5));

  val ywp = List(Point(5, 5), Point(10, 5), Point(0, 10))
  val y = new Edge(y1, y3, ywp);*/
  /**
   * --------|
   *         |
   *     ---------
   *     |   |
   *     |----
   *     |
   *------          
   * @return
   */

  @Test
  def intersectEdge() = {
  /*  val xwp = List(Point(15, 8), Point(5, 8), Point(0, 0))
    val x = new Edge(x1, x3, xwp)
    x.firstIntersection(y, Set()) match {
      case Some(Point(5, 5)) ⇒
      case x ⇒ fail(x.toString)
    }
    y.firstIntersection(x, Set()) match {
      case Some(Point(10, 8)) ⇒
      case x ⇒ fail(x.toString)
    }
    assertEquals(x.firstIntersection(y, Set(Point(5, 5))), Some(Point(10, 8)))
    val xbefore = new Edge(x1, y3, List(Point(5, 5), Point(0, 0)))
    val xafter = new Edge(y3, x3, List(Point(15, 8), Point(8, 5), Point(5, 5)))
    assertEquals(xbefore.firstIntersection(y, Set()), Some(Point(5, 5)))*/
  }
  /*@Test
  def testGraphIsec {
    val xwp = List(Point(15, 8), Point(5, 8), Point(0, 0))
    val x = new Edge(x1, x3, xwp)
    val g = new ConnectionGraphV(Set(x1,x3), Set(x))
    val g1= g.add(y1)
    assertEquals(g1.vertexs, Set(x1,x3,y1))
    assertEquals(g1.edges, Set(x))
    val g2 = g1.add(y3)
    assertEquals(g2.vertexs, Set(x1,x3,y1,y3))
    //assertEquals(g2.edges, Set(x))
    //val g3 = g2.add(y)
    //println("g3=" + g3.vertexs)
    //println("ge=" + g3.edges)
    //assert(g3.vertexs.exists { _.p == Point(5,5) })
    //assert(!g3.edges.contains(x))
  }
  @Test
  def cutTest {
    val xwp = List(Point(15, 8), Point(5, 8), Point(0, 0))
    val x = new Edge(x1, x3, xwp)
    val g = new ConnectionGraphV(Set(y1,y3),Set(y)).add(x1).add(x3)
    println("g=" + g.edges)
   // val res =g.cut(Set(x))
   // println(res)
  }*/
  @Test
  def interval {
    // s--<-->--e
    def test(s:Int,e:Int, s1:Int,e1:Int , solution:List[Int]) = {
      assertEquals(new Interval(s,e).intersect(new Interval(s1,e1),false), solution)
    }
    test(5,10 , 7,8,  List(7,8))
    test(5,10 , 7,15, List(7,10))
    test(5,10 , 2,6, List(5,6))
    test(10,5 , 7,10, List(10,7))
  }
  /*@Test
  def untangle {
   val a = new End(Point(0,5))
   val b = new End(Point(10,0))
   val c = new End(Point(5,10))
   val e = new Edge(a,c, List(Point(5,10),Point(10,0), Point(0,5)))
   e.untangle
  }*/
}