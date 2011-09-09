/*package org.zaluum.nide.scratch
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.ForkJoinTask
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.alg.StrongConnectivityInspector
import org.jgrapht.DirectedGraph
import scala.collection.JavaConversions._
import org.jgrapht.traverse.TopologicalOrderIterator
import scala.collection.mutable.Buffer
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.CountDownLatch
import java.util.concurrent.CyclicBarrier
import java.util.concurrent.Semaphore
object DAGAnalysys {
  case class Thread(num: Int) {
    var forkedBy: Option[Vertex] = None
    val instructions = Buffer[Vertex]()
    override def toString = "Thread " + num + " -> " + instructions.map(_.toInstructionsSeq).mkString(", ")
  }
  case class Vertex(s: String) {
    var expensive: Boolean = true
    var init : Boolean = false
    var thread: Thread = null
    val fork = Buffer[Thread]()
    val join = Buffer[Vertex]()
    var isJoinPoint = false
    def toInstructionsSeq = {
      val result = join.map { t ⇒ "Join(" + t.s + ")" }
      result += "Exec(" + s + ")"
      result ++= fork.map { t ⇒ "Fork(" + t.num + ")" }
      result.mkString(", ")
    }
  }
  class Edge {
  }
  type D = DirectedGraph[Vertex, Edge]
  def test1 = {
    
    val dag = new DirectedAcyclicGraph[Vertex, Edge](classOf[Edge])
      def v(s: String) = {
        val res = Vertex(s)
        dag.addVertex(res)
        res
      }
    val a = v("a")
    val b = v("b")
    val c = v("c")
    val d = v("d")
    val e = v("e")
    val f = v("f")
    val g = v("g")
    val h = v("h")
    val i = v("i")
    val j = v("j")
    val k = v("k")
    val l = v("l")
    val m = v("m")
    dag.vertexSet().foreach(_.expensive = false)
    //dag.addDagEdge(b, m)
    // dag.addDagEdge(m, c)
    // b.expensive = false
    // f.expensive = false
    dag.addDagEdge(a, b)
    dag.addDagEdge(b, c)
    dag.addDagEdge(c, d)
    dag.addDagEdge(d, e)
    dag.addDagEdge(a, f)
    dag.addDagEdge(f, g)
    dag.addDagEdge(g, h)
    dag.addDagEdge(f, i)
    dag.addDagEdge(g, j)
    dag.addDagEdge(j, k)
    dag.addDagEdge(j, l)
    dag.addDagEdge(k, e)
    dag
  }
  def test2 = {
    val dag = new DirectedAcyclicGraph[Vertex, Edge](classOf[Edge])
      def v(s: String) = {
        val res = Vertex(s)
        dag.addVertex(res)
        res
      }
    val a = v("a")
    val b = v("b")
    val c = v("c")
    val d = v("d")
    val e = v("e")
    val f = v("f")
    val g = v("g")
    val h = v("h")
    val i = v("i")
    val j = v("j")
    val k = v("k")
    val l = v("l")
    /*dag.vertexSet().foreach { _.expensive = false }
    a.expensive = true
    f.expensive = true*/
    List(a -> b, b -> c, c -> d, d -> e, e -> f, f -> g, b -> h, h -> d, h -> k, k -> l, k -> f, a -> i, i -> h, i -> j) foreach {
      case (fo, to) ⇒ dag.addDagEdge(fo, to)
    }
    dag
  }
  def test3 = {
    val dag = new DirectedAcyclicGraph[Vertex, Edge](classOf[Edge])
      def v(s: String) = {
        val res = Vertex(s)
        dag.addVertex(res)
        res
      }
    val a = v("a")
    val b = v("b")
    val c = v("c")
    a.expensive = false
    b.expensive = false
    List(c -> a, b -> a) foreach {
      case (fo, to) ⇒ dag.addDagEdge(fo, to)
    }
    dag
  }
  def runDag(dag: D) {
    var threads = Buffer[Thread]()
      
      def visitS(v: Vertex, thread: Thread) {
        if (v.thread == null) {
          v.thread = thread
          thread.instructions += v
          dag.outgoingEdgesOf(v).map(dag.getEdgeTarget).find(_.thread == null) foreach { next ⇒
            visitS(next, thread)
          }
        }
      }
    for (v ← new TopologicalOrderIterator(dag)) {
      if (v.thread == null) {
        val thread = new Thread(threads.size)
        threads += thread
        v.init=true
        visitS(v, thread)
      }
    }
    for (from ← dag.vertexSet(); i ← dag.outgoingEdgesOf(from); val to = dag.getEdgeTarget(i)) {
      if (from.thread!=to.thread) {
        if (to.init) {
          from.fork += to.thread
        }else {
          to.join += from
          from.isJoinPoint=true
        }
      }
    }
    threads foreach (println)
  }
  
  def main(args: Array[String]) {
    val dag = test3
    runDag(dag)
  }

}*/
/*object DAGAnalysys {
  case class Vertex(s: String) {
    var expensive: Boolean = true
    var topo: Int = 0
    val fork = Buffer[Thread]()
    val join = Buffer[Thread]()
    def toInstructionsSeq = {
     val result = join.map { t ⇒ "Join(" + t.num + ")" }
      result += "Exec(" + s + ")"
      result ++= fork.map { t ⇒ "Fork(" + t.num + ")" }
      result.mkString(", ")
    }
  }
  type D = DirectedGraph[Vertex, DefaultEdge]
  def incomingVertexsSorted(v: Vertex, dag: D) =
    dag.incomingEdgesOf(v).map { e ⇒ dag.getEdgeSource(e) }.toList.sortBy { _.topo }
  
  def test2 = {
    val dag = new DirectedAcyclicGraph[Vertex, DefaultEdge](classOf[DefaultEdge])
      def v(s: String) = {
        val res = Vertex(s)
        dag.addVertex(res)
        res
      }
    val a = v("a")
    val b = v("b")
    val c = v("c")
    val d = v("d")
    val e = v("e")
    val f = v("f")
    val g = v("g")
    val h = v("h")
    val i = v("i")
    val j = v("j")
    val k = v("k")
    val l = v("l")
    /*dag.vertexSet().foreach { _.expensive = false }
    a.expensive = true
    f.expensive = true*/
    List(a -> b, b -> c, c -> d, d -> e, e -> f, f -> g, b -> h, h -> d, h -> k, k -> l, k -> f, a -> i, i -> h, i -> j) foreach {
      case (fo, to) ⇒ dag.addDagEdge(fo, to)
    }
    dag
  }
  def test1 = {
    val dag = new DirectedAcyclicGraph[Vertex, DefaultEdge](classOf[DefaultEdge])
      def v(s: String) = {
        val res = Vertex(s)
        dag.addVertex(res)
        res
      }
    val a = v("a")
    val b = v("b")
    val c = v("c")
    val d = v("d")
    val e = v("e")
    val f = v("f")
    val g = v("g")
    val h = v("h")
    val i = v("i")
    val j = v("j")
    val k = v("k")
    val l = v("l")
    val m = v("m")
    dag.vertexSet().foreach(_.expensive = false)
    //dag.addDagEdge(b, m)
    // dag.addDagEdge(m, c)
    // b.expensive = false
    // f.expensive = false
    dag.addDagEdge(a, b)
    dag.addDagEdge(b, c)
    dag.addDagEdge(c, d)
    dag.addDagEdge(d, e)
    dag.addDagEdge(a, f)
    dag.addDagEdge(f, g)
    dag.addDagEdge(g, h)
    dag.addDagEdge(f, i)
    dag.addDagEdge(g, j)
    dag.addDagEdge(j, k)
    dag.addDagEdge(j, l)
    dag.addDagEdge(k, e)
    dag
  }

  case class Thread(num: Int) {
    var forkedBy: Option[Vertex] = None
    val instructions = Buffer[Vertex]()
    def isBefore(myVertex: Vertex, searchVertex: Vertex) = {
      val my = instructions.indexOf(myVertex)
      val toFind = instructions.indexOf(searchVertex)
      if (toFind != -1 && my <= toFind) true
      else false
    }
    override def toString = "Thread " + num + " -> " + instructions.map(_.toInstructionsSeq).mkString(", ")
  }
  def createThreads(dag: D) = {
    val order = new TopologicalOrderIterator(dag).toList
    val threads = Buffer[Thread]()
    for ((v, i) ← order.zipWithIndex) {
      v.topo = i
    }
      def threadOf(v: Vertex) = threads.find { t ⇒ t.instructions.contains(v) }.get
      def pathToRoot(v: Vertex, p: Buffer[Vertex]) { // improve
        p += v
        val thread = threadOf(v)
        val next = thread.instructions.indexOf(v) - 1
        if (next != -1) {
          pathToRoot(thread.instructions(next), p)
        } else {
          thread.forkedBy match {
            case Some(v) ⇒ pathToRoot(v, p)
            case None    ⇒
          }

        }
      }
    for (v ← order) {
      if (dag.inDegreeOf(v) == 0) { // if no dependencies, create thread
        val newThread = Thread(threads.size)
        newThread.forkedBy = None
        newThread.instructions += v
        threads += newThread
      } else { // has dependencies
        val incomings = incomingVertexsSorted(v, dag)
        // all the incoming vertexs have been visited due to topo order
        val forkInstruction = incomings.last // attach to the last in topo order
        val execThread = threadOf(forkInstruction)
        // if it's the last instruction no need to fork 
        if (execThread.instructions.last == forkInstruction) {
          execThread.instructions += v
        } else { // fork
          val newThread = Thread(threads.size)
          incomings.last.fork += newThread
          newThread.forkedBy = Some(forkInstruction)
          newThread.instructions += v
          threads += newThread
        }
        // see if we have to join spawned threads to evaluate this instruction
        val backEdges = incomings.dropRight(1)
        val path = Buffer[Vertex]()
        pathToRoot(v, path)
        for (bv ← backEdges) {
          if (!path.contains(bv)) { // executed in parallel somewhere so join
            val threadToJoin = threadOf(bv)
            // join only if not joined already
            if (!execThread.instructions.exists(_.join.contains(threadToJoin)))
              v.join += threadToJoin
          }
        }
      }
    }
    threads
  }
  def inlineNonExpensiveThreads(threads: List[Thread]) = {
    val removedThreads = Buffer[Thread]()
      def resultThreads = threads.filterNot(removedThreads.contains(_))
      def migrateJoins(from: Thread, to: Thread) {
        println("migrating joins " + from.num + " to " + to.num)
        for (t ← threads; i ← t.instructions) {
          if (i.join.contains(t)) {
            i.join -= t
          }
          if (i.join.contains(from)) {
            println("found " + from.num + " in " + i)
            i.join -= from
            if (t != to) i.join += to
          }
        }
      }
      def threadOf(v: Vertex) = resultThreads
        .find { t ⇒ t.instructions.contains(v) }.get
      def print = for (t ← resultThreads.sortBy { _.num }) println(t)

    for (t ← threads) {
      if (t.instructions forall (!_.expensive)) { // inlinable
        println("*** merging " + t.num)
        // TODO join at the earliest joiner possible point
        t.forkedBy match {
          case Some(forker) ⇒
            forker.fork -= t
            val toThread = threadOf(forker)
            toThread.instructions ++= t.instructions
            migrateJoins(t, toThread)
            removedThreads += t
          case None ⇒
            if (t != threads(0)) {
              threads(0).instructions.prependAll(t.instructions) // zero thread
              migrateJoins(t, threads(0))
              removedThreads += t
            }
        }
      }
      print
      println("***")

    }
    println("final")
    print

  }
  def main(args: Array[String]) {
    val dag = test2
    val threads = createThreads(dag)
    for (t ← threads.sortBy { _.num }) println(t)
    val finalThreads = inlineNonExpensiveThreads(threads.toList)
    /*println("*** inline")*/

  }
} */

/*
object DAGAnalysys {
  case class Vertex(s: String) {
    var expensive: Boolean = true
    var set: Int = 0
    var executed = false
  }
  type D = DirectedGraph[Vertex, DefaultEdge]

  def markDag(dag: D) = {
    var current = 1
      def mark(v: Vertex, parallelRunning: Boolean, depth: Int): Boolean = { // returns true if expsensive Branch
        val nextVertexs = dag.outgoingEdgesOf(v).map { e ⇒ dag.getEdgeTarget(e) }
        //println((" " * depth) + "analyzing " + v + " parallelRunning " + parallelRunning + nextVertexs)
        if (v.set == 0) {
          val newParallelRunning = if (v.expensive && parallelRunning) {
            current += 1; false
          } else
            parallelRunning
          v.set = current
          var expensiveChildren = false
          for (nextV ← nextVertexs)
            expensiveChildren |= mark(nextV, expensiveChildren || newParallelRunning, depth + 1)
          expensiveChildren || v.expensive
        } else false
      }
    val inits = dag.vertexSet().filter(v ⇒ dag.inDegreeOf(v) == 0)
    var parallelRunning = false
    for (v ← inits) {
      parallelRunning |= mark(v, parallelRunning, 0)
    }
    current
  }
  def emitFJ(dag: D, maxSet: Int) {
    val order = new TopologicalOrderIterator(dag).toList
    var joined = Set[Int]()
    var map = Map[Int, String]()
      def add(set: Int, action: String) = {
        val newValue = map.get(set).map(_ + " " + action).getOrElse(action)
        map += (set -> newValue)
      }
      def emitSet(set: Int) {
        for (v ← order.filter { _.set == set }) {
          val incomingVertexs = dag.incomingEdgesOf(v) map { e ⇒ dag.getEdgeSource(e) }
          for (iv ← incomingVertexs; if (iv.set > v.set)) {
            joined += iv.set
            add(v.set, "JOIN(" + iv.set + ")")
          }
          add(v.set, "EXEC(" + v.s + ")")
          val outgoingVertexs = dag.outgoingEdgesOf(v) map { e ⇒ dag.getEdgeTarget(e) }
          for (ov ← outgoingVertexs) {
            if (ov.set > v.set) {
              add(v.set, "FORK(" + ov.set + ")")
            }
          }
        }
      }
    for (i ← 1 to maxSet) emitSet(i)
    for (i ← 2 to maxSet; if !joined.contains(i)) add(1, "JOIN(" + i + ")")
    map.toList.sortBy { _._1 } foreach { println }
  }
  
  def main(args: Array[String]) {
    val dag = test3
    for (v ← dag.vertexSet) {
      val to = dag.outgoingEdgesOf(v) map { e ⇒ dag.getEdgeTarget(e) } mkString (",")
      println(v + "->" + to)
    }
    val maxSet = markDag(dag)
    println("result")
    dag.vertexSet().toList.sortBy(_.s).foreach { v ⇒ println(v.s + " " + v.set) }
    emitFJ(dag, maxSet)
  }
}*/
/*
object Executors2 {
  def task(body: ⇒ Unit) = new Runnable() {
    def run() {
      body
    }
  }

  val exec = Executors.newCachedThreadPool();
  val helloSaidJoin = new Semaphore(0);
  val cridat = new Semaphore(0);
  val rebre = task {
    cridat.acquire()
    println("pasi pasi")
    helloSaidJoin.release()
    println("ok saludat")
  }
  def main(args: Array[String]) {
    println("que hi ha algu?")
    exec.execute(rebre)
    cridat.release()
    helloSaidJoin.acquire()
    println("paso")
    exec.shutdown()
  }
}
object ForkJoin2 {
  def fj(body: ⇒ Unit) = ForkJoinTask.adapt(new Runnable() {
    def run() {
      body
    }
  })
  val task2 = fj {
    println("F")
    task4.fork
    println("G")
    task3.fork
    println("H")
  }
  val task3 = fj {
    println("J")
    task5.fork
    println("K")

  }
  val task4 = fj {
    println("I")
  }
  val task5 = fj {
    println("L")
  }
  def main(args: Array[String]) {
    val fp = new ForkJoinPool
    println("A")
    fp.execute(task2)
    println("BCD")
    task3.join
    println("E")
    task2.join
    task4.join
    task5.join
  }
}*/ 