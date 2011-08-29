package org.zaluum.nide.compiler

sealed trait StorageType
object StorageLocal extends StorageType
object StorageValField extends StorageType
object StorageJoinField extends StorageType

class AnalyzerParallelism(bl: BlockSymbol, a: Analyzer) {
  import scala.collection.JavaConversions._
  def run() {

    val dag = bl.dag
    val threads = bl.threads
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
    // mark thisVal storage
    val thisVal = bl.template.thisVal
    val mainVal = a.toCompile.sym.thisVal
    for (pi ← thisVal.portInstances) {
      if (pi.isField)
        pi.internalStorage = StorageValField
      else {
        val connectedVals =
          bl.connections.outgoingConnections(pi)
            .map { _.valSymbol }
        val internalConnectedVals = connectedVals.filter { _ != thisVal };
        // mark arguments to be stored as field if needed by another thread            
        if (internalConnectedVals.exists(_.thread.num != 0)) { // sure it's In or Shift 
          assert(pi.dir == In || pi.dir == Shift)
          assert(pi.internalStorage != StorageValField)
          pi.internalStorage = StorageJoinField
        }
      }
    }

    // mark outs and shifts as StorageJoinsIf needed by another thread
    for (vs ← bl.executionOrder; pi ← vs.portInstances) {
      val connectedVals =
        bl.connections.outgoingConnections(pi)
          .map { _.valSymbol }
      val isThisValConnected = connectedVals.exists(_ == thisVal)
      if (isThisValConnected)
        pi.internalStorage = StorageJoinField
      else if (pi.portSymbol.map(_.isField).getOrElse(false))
        pi.internalStorage = StorageValField
      else if (pi.dir == Out || pi.dir == Shift)
        pi.internalStorage = if (vs.isJoinPoint) StorageJoinField else StorageLocal
    }

    for (vs ← dag.vertexSet()) {
      vs.tpe match {
        case t: TemplateExprType ⇒
          for (bl ← t.blocks) {
            new AnalyzerParallelism(bl, a).run()
          }
        case _ ⇒
      }
    }
  }

}
