package org.zaluum.nide.compiler

sealed trait StorageType
object StorageLocal extends StorageType
object StorageValField extends StorageType
object StorageJoinField extends StorageType

class AnalyzerParallelism(bl: BlockSymbol, a: Analyzer) {
  import scala.collection.JavaConversions._
  val singleThreaded = true
  def run() {
    val dag = bl.dag
    val paths = bl.execPaths

    if (singleThreaded) {
      val execPath = new ExecutionPath(0, bl)
      paths += execPath
      for (v ← bl.executionOrder) {
        v.execPath = execPath
        execPath.instructions :+= v
      }
    } else {
        def visitS(v: ValSymbol, execPath: ExecutionPath) {
          if (v.execPath == null) {
            v.execPath = execPath
            execPath.instructions :+= v
            dag.outgoingEdgesOf(v).map(dag.getEdgeTarget).find(_.execPath == null) foreach { next ⇒
              visitS(next, execPath)
            }
          }
        }
      for (v ← bl.executionOrder) {
        if (v.execPath == null) {
          val execPath = new ExecutionPath(paths.size, bl)
          paths += execPath
          v.init = true
          visitS(v, execPath)
        }
      }
      if (paths.size == 0) paths += new ExecutionPath(0, bl)
      for (from ← dag.vertexSet(); i ← dag.outgoingEdgesOf(from); val to = dag.getEdgeTarget(i)) {
        if (from.execPath != to.execPath) {
          if (to.init) {
            from.fork += to.execPath
            to.execPath.forkedBy = Some(from)
          } else {
            to.join += from
            from.isJoinPoint = true
          }
        }
      }
    }

    // mark thisVal storage
    val thisVal = bl.template.thisVal
    if (thisVal.execPath == null) thisVal.execPath = paths(0)

    val mainVal = a.toCompile.sym.thisVal
    for (pi ← thisVal.portInstances) {
      if (pi.internalStorage != StorageValField) {
        if (pi.isField) {
          pi.internalStorage = StorageValField
        } else {
          if (pi.dir == In || pi.dir == Shift) {
            val connectedVals = bl.connections.outgoingConnections(pi).map { _.valSymbol }
            val internalConnectedVals = connectedVals.filter { _ != thisVal }; // filter auto connections |--->|
            if (internalConnectedVals.exists(_.execPath.num != 0)) {
              pi.internalStorage = StorageJoinField
            }
          }
          if (pi.dir == Out || pi.dir == Shift) {
            bl.connections.connectedFrom.get(pi).map { _._1.valSymbol } match {
              case Some(from) if from != thisVal && from.execPath.num != 0 ⇒
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
          pi.internalStorage = StorageValField
        } else {
          val connectedToPaths =
            bl.connections.outgoingConnections(pi).map { _.valSymbol.execPath }
          val connectedFromPaths =
            bl.connections.connectedFrom.get(pi).map { _._1.valSymbol.execPath }
          val connectedPaths = connectedToPaths ++ connectedFromPaths
          if (connectedPaths.exists { to ⇒
            to != vs.execPath && !(vs.execPath.num == 0 && to == thisVal.execPath)
          })
            pi.internalStorage = StorageJoinField
        }
      }
    }
    for (vs ← dag.vertexSet()) {
      vs.tpe match {
        case Some(t: TemplateExprType) ⇒
          for (bl ← vs.blocks) {
            new AnalyzerParallelism(bl, a).run()
          }
        case _ ⇒
      }
    }
  }

}
