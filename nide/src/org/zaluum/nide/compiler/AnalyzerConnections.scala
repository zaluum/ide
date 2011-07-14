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
          def errorDag(str: String) {
            error(str, c.findConnectionFor(vin) orElse (c.findConnectionFor(vout))
              getOrElse (c.connections.head))
          }
          try {
            acyclic.addDagEdge(vout.valSymbol, vin.valSymbol);
          } catch {
            case e: CycleFoundException ⇒ errorDag("Cycle found.")
            case e: IllegalArgumentException ⇒ errorDag("Loop connection found. Cannot connect a box to itself.")
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
        for (in <- ins) {
          in.connectedFrom = Some(out)
          in.blameConnection = c.findConnectionFor(in)
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
          if (pi.finalTpe == NoSymbol) error("Cannot find type of port " + pi.name.str, vs.decl)
          pi.connectedFrom foreach { from =>
            if (!checkAssignmentPossible(from.finalTpe, pi.finalTpe)) {
              error("Connection with incompatible types", pi.blameConnection.get)
            }
          }
        }
      }
      def checkBinExprTypes(vs: ValSymbol) {
        import primitives._
        val s = vs.tpe.asInstanceOf[BinExprType]
        val (a, b, o) = s.binaryPortInstancesOf(vs)
        def fromTpe(p: RealPortInstance) = p.connectedFrom.map(_.finalTpe).getOrElse(NoSymbol)
        def assignAll(tpe: Type) = {
          a.finalTpe = tpe
          b.finalTpe = tpe
          s match {
            case _: MathExprType => o.finalTpe = tpe
            case _: CmpExprType => o.finalTpe = primitives.Boolean
          }
        }
        (fromTpe(a), fromTpe(b)) match {
          case (NoSymbol, NoSymbol) =>
            assignAll(Int)
          case (NoSymbol, bt) =>
            if (isNumeric(bt))
              assignAll(toOperationType(unbox(bt)))
          case (at, NoSymbol) =>
            if (isNumeric(at))
              assignAll(toOperationType(unbox(at)))
          case (at, bt) =>
            if (isNumeric(at) && isNumeric(bt)) {
              val ao = toOperationType(unbox(at))
              val bo = toOperationType(unbox(bt))
              assignAll(largerOperation(ao, bo))
            }
        }
        def checkNumeric(p: RealPortInstance) {
          val tpe = fromTpe(p)
          if (!isNumeric(tpe) && tpe != NoSymbol)
            error("Type " + tpe.fqName.str + " is not numeric ", p.blameConnection.get)
        }
        checkNumeric(a)
        checkNumeric(b)

      }
      def checkCastExprTypes(vs: ValSymbol) {
        import primitives._
        val e = vs.tpe.asInstanceOf[CastExprType]
        val (a, o) = e.unaryPortInstancesOf(vs)
        e match {
          case ToByteType => o.finalTpe = Byte
          case ToShortType => o.finalTpe = Short
          case ToCharType => o.finalTpe = Char
          case ToIntType => o.finalTpe = Int
          case ToLongType => o.finalTpe = Long
          case ToFloatType => o.finalTpe = Float
          case ToDoubleType => o.finalTpe = Double
        }
        a.connectedFrom.map(_.finalTpe) match {
          case Some(t) => t match {
            case j: PrimitiveJavaType if isNumeric(j) => a.finalTpe = j
            case _ => a.finalTpe = o.finalTpe; error("Cast between incompatible types", a.blameConnection.get)
          }
          case None => a.finalTpe = o.finalTpe
        }
      }
      def checkLiteralExprType(vs: ValSymbol) {
        val l = LiteralExprType
        val o = l.outPort(vs)
        val t = vs.params.headOption match {
          case Some((p, vuntrimmed: String)) =>
            Literals.parseNarrowestLiteral(vuntrimmed.trim)
          case e => None
        }
        t match {
          case Some((_,tpe)) => 
            o.finalTpe=tpe
          case None => 
            o.finalTpe=NoSymbol; 
            error("Cannot parse literal" , vs.decl)
        }
      }
      
      def checkTypes() {
        bs.thisVal.portInstances foreach { pi =>
          pi.asInstanceOf[RealPortInstance].finalTpe = pi.tpe
        }
        for (vs <- bs.valsInOrder) {
          vs.tpe match {
            case bs: BoxTypeSymbol => checkBoxTypes(vs)
            case b: BinExprType => checkBinExprTypes(vs)
            case e: CastExprType => checkCastExprTypes(vs)
            case LiteralExprType => checkLiteralExprType(vs)
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
