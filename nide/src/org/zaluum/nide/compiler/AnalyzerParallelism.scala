package org.zaluum.nide.compiler

sealed trait StorageType
object StorageLocal extends StorageType
object StorageValField extends StorageType
object StorageJoinField extends StorageType

class AnalyzerParallelism(bl: BlockSymbol, a: Analyzer) {
  import scala.collection.JavaConversions._
  val singleThreaded = false
  def run() {
    val dag = bl.dag
    val threads = bl.threads

    if (singleThreaded) {
      val thread = new ZThread(0, bl)
      threads += thread
      for (v ← bl.executionOrder) {
        v.thread = thread
        thread.instructions :+= v
      }
    } else {
        def visitS(v: ValSymbol, thread: ZThread) {
          if (v.thread == null) {
            v.thread = thread
            thread.instructions :+= v
            dag.outgoingEdgesOf(v).map(dag.getEdgeTarget).find(_.thread == null) foreach { next ⇒
              visitS(next, thread)
            }
          }
        }
      for (v ← bl.executionOrder) {
        if (v.thread == null) {
          val thread = new ZThread(threads.size, bl)
          threads += thread
          v.init = true
          visitS(v, thread)
        }
      }
      if (threads.size == 0) threads += new ZThread(0, bl)
      for (from ← dag.vertexSet(); i ← dag.outgoingEdgesOf(from); val to = dag.getEdgeTarget(i)) {
        if (from.thread != to.thread) {
          if (to.init) {
            from.fork += to.thread
            to.thread.forkedBy = Some(from)
          } else {
            to.join += from
            from.isJoinPoint = true
          }
        }
      }
    }

    // mark thisVal storage
    val thisVal = bl.template.thisVal
    if (thisVal.thread == null) thisVal.thread = threads(0)

    val mainVal = a.toCompile.sym.thisVal
    for (pi ← thisVal.portInstances) {
      if (pi.internalStorage != StorageValField) {
        if (pi.isField) {
          pi.internalStorage = StorageValField
        } else {
          if (pi.dir == In || pi.dir == Shift) {
            val connectedVals =
              bl.connections.outgoingConnections(pi)
                .map { _.valSymbol }
            val internalConnectedVals = connectedVals.filter { _ != thisVal }; // filter auto connections |--->|
            if (internalConnectedVals.exists(_.thread.num != 0)) { // sure it's In or Shift 
              pi.internalStorage = StorageJoinField
            }
          }
          if (pi.dir == Out || pi.dir == Shift) {
            val connectedFrom = bl.connections.connectedFrom.get(pi).map { _._1.valSymbol }
            connectedFrom match {
              case Some(from) if (from != thisVal) && from.thread.num != 0 ⇒
                pi.internalStorage = StorageJoinField
              case _ ⇒
            }
          }
        }
        // mark arguments to be stored as field if needed by another thread            
      }
    }

    for (vs ← bl.executionOrder; pi ← vs.portInstances) {
      if (pi.internalStorage != StorageValField) {
        val isField = pi.portSymbol.map(_.isField).getOrElse(false)
        if (isField) {
          pi.internalStorage == StorageValField
        } else {
          val connectedToThreads =
            bl.connections.outgoingConnections(pi).map { _.valSymbol.thread }
          val connectedFromThreads =
            bl.connections.connectedFrom.get(pi).map { _._1.valSymbol.thread }
          val connectedThreads = connectedToThreads ++ connectedFromThreads
          if (connectedThreads.exists(_ != vs.thread)) {
            pi.internalStorage = StorageJoinField
          }
        }
      }
    }
    for (vs ← dag.vertexSet()) {
      vs.tpe match {
        case t: TemplateExprType ⇒
          for (bl ← vs.blocks) {
            new AnalyzerParallelism(bl, a).run()
          }
        case _ ⇒
      }
    }
  }

}
