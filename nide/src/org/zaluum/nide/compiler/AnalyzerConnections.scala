package org.zaluum.nide.compiler
import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge

trait AnalyzerConnections {
  self: Analyzer =>

  class CheckConnections(b: BoxDef, owner: Symbol) {
    val bs = b.sym
    val acyclic = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
    var usedInputs = Set[RealPortInstance]()
    def check() = Checker.traverse(b)

    object Checker extends Traverser(owner) with ReporterAdapter {
      def location(tree: Tree) = globLocation(tree)
      def reporter = self.reporter
      // parse connections
      override def traverse(tree: Tree) {
        tree match {
          case b: BoxDef ⇒
            traverseTrees(b.vals)
            traverseTrees(b.junctions)
            traverseTrees(b.connections)
            b.defs foreach {
              case bDef: BoxDef ⇒
                new CheckConnections(bDef, b.symbol).check()
            }
            check()
          case v: ValDef ⇒ acyclic.addVertex(v.sym) // valdefs always have symbol
          case j @ Junction(name, _) ⇒
            bs.connections.lookupJunction(name) match {
              case Some(j) ⇒ error("junction name already exists", j)
              case None ⇒ bs.connections.junctions += j
            }
          case c @ ConnectionDef(a, b, waypoints) ⇒
            if (a == EmptyTree || b.symbol == EmptyTree) {
              error("incomplete connection " + a + "<->" + b, tree)
            } else {
              bs.connections.addConnection(c)
            }
          case _ ⇒
        }
      }
      // check
      def check() {
        // 1 - Connections go to known ports
        val resolved = b.connections.forall {
          case con: ConnectionDef ⇒
            def checkResolved(p: Tree) = p match {
              case EmptyTree ⇒ error("Wire is not connected", con); false
              case j: JunctionRef ⇒ /*if(!bs.junctions.exists {_.name == j.name}) {
                  error("FATAL: junction does not exists " + j,con)
                }*/ true
              case p: PortRef ⇒
                PortSide.find(p, bs) match {
                  case None ⇒ error("Cannot find port " + p, con); false
                  case _ ⇒ true
                }
            }
            checkResolved(con.a) && checkResolved(con.b)
        }
        if (resolved && reporter.errors.isEmpty) {
          bs.connections.clumps foreach { checkClump(_) }
          checkTypes();
          bs.connections.clumps foreach { putConnectionTypes(_) }
          import scala.collection.JavaConversions._
          bs.executionOrder = new TopologicalOrderIterator(acyclic).toList
        }
      }
      def checkClump(c: Clump) {
        val ins = c.ports.filter(p ⇒ p.flowIn) map { _.realPi } // TODO stablish when realPi is ensured
        val outs = c.ports.filter(p ⇒ !p.flowIn) map { _.realPi }
        if (outs.size == 0) error("No output connected", c.connections.head)
        else if (outs.size > 1) error("More than one output is connected", c.connections.head)
        else if (ins.size == 0) error("No inputs connected", c.connections.head)
        else if (!usedInputs.intersect(ins).isEmpty) error("input connected multiple times", c.connections.head) // TODO check online to identify offending connection 
        else {
          checkGraphFlow(c, ins, outs.head)
          putTypes(c, ins, outs.head)
        }
      }
      def checkGraphFlow(c: Clump, ins: Set[RealPortInstance], out: RealPortInstance) {
        // check graph consistency
        bs.connections.flow += (out -> ins)
        def addDag(vout: PortInstance, vin: PortInstance) {
          try {
            acyclic.addDagEdge(vout.valSymbol, vin.valSymbol);
          } catch {
            case e: CycleFoundException ⇒ error("cycle found ", c.connections.head)
            case e: IllegalArgumentException ⇒ error("loop found", c.connections.head)
          }
        }
        import org.zaluum.nide.RichCast._
        for (in ← ins) {
          if (!isInside(out) && !isInside(in))
            addDag(out, in)
        }

      }
      def isInside(p: PortInstance) = p.valSymbol == bs.thisVal

      def putTypes(c: Clump, ins: Set[RealPortInstance], out: RealPortInstance) {
        usedInputs ++= ins
        // check types
        for (pi <- ins) {
          pi.connectedFrom = Some(out)
        }
      }
      def checkAssignmentPossible(from: Type, to: Type): Boolean = {
        from match {
          case f: PrimitiveJavaType =>
            to match {
              case t: PrimitiveJavaType if t == f => true
              case t: PrimitiveJavaType => primitives.widening(f, t)
              case _ => false
            }
          case f: JavaType => f == to
          case _ => false
        }
      }
      def checkBoxTypes(vs: ValSymbol) {
        for (api <- vs.portInstances; val pi = api.asInstanceOf[RealPortInstance]) {
          pi.finalTpe = pi.tpe
          if (pi.finalTpe == NoSymbol) error("Cannot find type ", vs.decl) // fixme better tree (connection)
          pi.connectedFrom foreach { from =>
            if (!checkAssignmentPossible(from.finalTpe, pi.finalTpe)) {
              error("Invalid assignment", vs.decl) // fixme error on connection
            }
          }
        }
      }
      def checkBinExprTypes(vs: ValSymbol) {
        val s = vs.tpe.asInstanceOf[ExprType]
        import primitives._
        val (a, b, c) = s.portInstancesOf(vs)
        def fromTpe(p: RealPortInstance) = p.connectedFrom.map(_.finalTpe).getOrElse(NoSymbol)
        def assignAll(tpe: Type) = {
          a.finalTpe = tpe
          b.finalTpe = tpe
          c.finalTpe = tpe
        }
        (fromTpe(a), fromTpe(b)) match {
          case (NoSymbol, NoSymbol) =>
            assignAll(Int)
          case (NoSymbol, bt) =>
            if (isNumeric(bt))
              assignAll(toOperationType(unbox(bt)))
            else
              error("Wrong type " + b, vs.decl) // fixme
          case (at, NoSymbol) =>
            if (isNumeric(at))
              assignAll(toOperationType(unbox(at)))
            else
              error("Wrong type " + a, vs.decl) // fixme
          case (at, bt) =>
            if (isNumeric(at) && isNumeric(bt)) {
              val ao = toOperationType(unbox(at))
              val bo = toOperationType(unbox(bt))
              assignAll(largerOperation(ao, bo))
            } else
              error("Wrong type " + a + b, vs.decl) // fixme
        }
      }
      def checkTypes() {
        bs.thisVal.portInstances foreach { pi =>
          pi.asInstanceOf[RealPortInstance].finalTpe = pi.tpe
        }
        for (vs <- bs.valsInOrder) {
          vs.tpe match {
            case bs: BoxTypeSymbol => checkBoxTypes(vs)
            case s: ExprType => checkBinExprTypes(vs)
          }
        }
        checkBoxTypes(bs.thisVal)
      }
      def putConnectionTypes(c: Clump) {
        val outO = c.ports.find(p ⇒ !p.flowIn) map { _.realPi }
        outO foreach { out =>
          for (con <- c.connections) { con.tpe = out.finalTpe }
          for (jun <- c.junctions) { jun.tpe = out.finalTpe }
        }
      }
    }
  }
}
