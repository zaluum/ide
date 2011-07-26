package org.zaluum.nide.compiler
import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.InvocationSite
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.ProblemMethodBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitScope
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumCompletionEngine
import org.eclipse.jdt.internal.core.builder.NameEnvironment
import org.eclipse.jdt.internal.compiler.util.ObjectVector
import org.eclipse.jdt.internal.core.SearchableEnvironment
import org.eclipse.jdt.internal.core.JavaProject
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.internal.core.CompilationUnit
import org.eclipse.jdt.internal.core.builder.SourceFile
import org.eclipse.jdt.internal.core.JavaModel
import org.eclipse.jdt.internal.core.JavaModelManager
import org.zaluum.nide.eclipse.integration.model.PreParsedZaluumCompilationUnit
import org.zaluum.nide.eclipse.integration.model.{ ZaluumCompilationUnitDeclaration, ZaluumTypeDeclaration }
import org.eclipse.jdt.internal.core.BasicCompilationUnit

trait AnalyzerConnections {
  self: Analyzer =>

  class CheckConnections(b: BoxDef, owner: Symbol) {
    val bs = b.sym
    val acyclic = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
    var usedInputs = Set[PortInstance]()
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
        // 1 - Check clumps. Do not check for port existence.
        for (c <- bs.connections.clumps) { checkClump(c) }
        // 4 - Propagate and check types
        checkTypes();
        // 5- Put calculated types to the clump
        for (c <- bs.connections.clumps) storeTypesInConnectionsAndJunctions(c)
      }
      def checkClump(c: Clump) {
        val ins = c.ports.filter(p ⇒ p.flowIn) map { _.pi }
        val outs = c.ports.filter(p ⇒ !p.flowIn) map { _.pi }
        if (outs.size == 0) error("No output connected", c.connections.head)
        else if (outs.size > 1) error("More than one output is connected", c.connections.head)
        else if (ins.size == 0) error("No inputs connected", c.connections.head)
        else if (!usedInputs.intersect(ins).isEmpty) error("input connected multiple times", c.connections.head) // TODO check online to identify offending connection 
        else {
          checkGraphFlow(c, ins, outs.head)
          storeFlow(c, ins, outs.head)
        }
      }
      def checkGraphFlow(c: Clump, ins: Set[PortInstance], out: PortInstance) {
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
        import scala.collection.JavaConversions._
        bs.executionOrder = new TopologicalOrderIterator(acyclic).toList
      }
      def isInside(p: PortInstance) = p.valSymbol == bs.thisVal
      def storeFlow(c: Clump, ins: Set[PortInstance], out: PortInstance) {
        usedInputs ++= ins
        for (in <- ins) {
          in.connectedFrom = Some(out)
          in.blameConnection = c.findConnectionFor(in)
        }
      }
      def checkGhostPorts(vs: ValSymbol) {
        for (pi <- vs.portInstances) {
          if (pi.missing) error("Ghost port. Cannot find port " + pi, vs.decl)
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
        for (pi <- vs.portInstances) {
          if (pi.missing) {
            error("Cannot find type of port " + pi.name.str, vs.decl)
          } else {
            pi.finalTpe = pi.portSymbol.get.tpe // I'm a box so if not missing I have portsymbol

            pi.connectedFrom foreach { from =>
              if (!checkAssignmentPossible(from.finalTpe, pi.finalTpe)) {
                error("Connection with incompatible types", pi.blameConnection.get)
              }
            }
          }
        }
      }
      def fromTpe(p: PortInstance) = p.connectedFrom.map(_.finalTpe).getOrElse(NoSymbol)
      def checkBinExprTypes(vs: ValSymbol) {
        import primitives._
        val s = vs.tpe.asInstanceOf[BinExprType]
        val (a, b, o) = s.binaryPortInstancesOf(vs)
        def assignAll(tpe: Type, outTpe: Type) = {
          a.finalTpe = tpe
          b.finalTpe = tpe
          o.finalTpe = outTpe
        }

        val at = fromTpe(a)
        val bt = fromTpe(b)
        val (one, other) = (at, bt) match {
          case (NoSymbol, NoSymbol) => (None, None)
          case (NoSymbol, bt) => (Some(bt), None)
          case (at, NoSymbol) => (Some(at), None)
          case (at, bt) => (Some(at), Some(bt))
        }

        s match {
          case b: BitBinExprType =>
            (one, other) match {
              case (Some(primitives.Boolean), Some(primitives.Boolean)) => assignAll(Boolean, Boolean)
              case (Some(primitives.Boolean), None) => assignAll(Boolean, Boolean)
              case (Some(p), None) if isIntNumeric(p) => assignAll(Int, Boolean)
              case (Some(p), Some(p2)) if isIntNumeric(p) && isIntNumeric(p2) => assignAll(Int, Boolean)
              case (None, _) => assignAll(Int, Boolean)
              case _ => error("Incompatible types", vs.decl)
            }
          case s: ShiftExprType =>
            if (isIntNumeric(bt) || bt == NoSymbol) {
              if (isIntNumeric(at) || at == NoSymbol) {
                assignAll(Int, Int)
              } else if (at == Long) {
                a.finalTpe = Long; b.finalTpe = Int; o.finalTpe = Long
              } else
                error("Shift only operates on Int and Long", a.blameConnection.get)

            } else error("Shift distance must be of Int type", b.blameConnection.get)
          case c: CmpExprType =>
            (one, other) match {
              case (Some(p1), None) if isNumeric(p1) => assignAll(toOperationType(unbox(p1)), Boolean)
              case (Some(p1), Some(p2)) if isNumeric(p1) && isNumeric(p2) => assignAll(toOperationType(unbox(p1)), Boolean)
              case (None, _) => assignAll(Int, Boolean)
              case _ => error("Incompatible types", vs.decl)
            }
          case e: EqualityExprType =>
            (one, other) match {
              case (Some(p1), None) if isNumeric(p1) => assignAll(toOperationType(unbox(p1)), Boolean)
              case (Some(p1), Some(p2)) if isNumeric(p1) && isNumeric(p2) => assignAll(toOperationType(unbox(p1)), Boolean)
              case (None, _) => assignAll(Int, Boolean)
              case (Some(p1), None) if p1 == primitives.Boolean => assignAll(Boolean, Boolean)
              case (Some(p1), Some(p2)) if p1 == p2 => assignAll(p1, Boolean)
              case _ => error("Incompatible types", vs.decl)
            }
          case _ =>
            (one, other) match {
              case (Some(p1), None) if isNumeric(p1) => val t = toOperationType(unbox(p1)); assignAll(t, t)
              case (Some(p1), Some(p2)) if isNumeric(p1) && isNumeric(p2) =>
                val t = largerOperation(toOperationType(unbox(p1)), toOperationType(unbox(p2)))
                assignAll(t, t)
              case (None, _) => assignAll(Int, Int)
              case _ => error("Incompatible types", vs.decl)
            }
        }
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
            p.tpe = primitives.String
            val v = vuntrimmed.trim
            Literals.parseNarrowestLiteral(v) match {
              case Some((_, tpe)) => o.finalTpe = tpe
              case None => error("Cannot parse literal " + v, vs.decl)
            }
          case e =>
            o.finalTpe = primitives.Byte;
        }
      }
      def checkUnaryExprType(vs: ValSymbol) {
        import primitives._
        val e = vs.tpe.asInstanceOf[UnaryExprType]
        val (a, o) = e.unaryPortInstancesOf(vs)
        e match {
          case e: CastExprType => checkCastExprTypes(vs)
          case MinusExprType =>
            fromTpe(a) match {
              case p if isNumeric(p) =>
                val t = toOperationType(unbox(p))
                a.finalTpe = t; o.finalTpe = t
              case NoSymbol => a.finalTpe = Int; o.finalTpe = Int
              case _ => error("Incompatible type", a.blameConnection.get)
            }
          case NotExprType =>
            fromTpe(a) match {
              case Boolean => a.finalTpe = Boolean; o.finalTpe = Boolean
              case p if isIntNumeric(p) => a.finalTpe = Int; o.finalTpe = Int
              case NoSymbol => a.finalTpe = Boolean; o.finalTpe = Boolean
              case _ => error("Incompatible type", a.blameConnection.get)
            }
        }
      }
      def checkInvokeExprType(vs: ValSymbol) {
        val obj = vs.findPortInstance(InvokeExprType.obj).get
        InvokeExprType.signatureSymbol.tpe = primitives.String // XXX ugly
        obj.connectedFrom match {
          case Some(from) =>
            from.finalTpe match {
              case c: ClassJavaType => invoke(vs, obj, c)
              case _ => error("bad type", vs.decl)
            }
          case None => // not connected
        }
      }

      def invoke(vs: ValSymbol, obj: PortInstance, c: ClassJavaType) {
        obj.finalTpe = c
        vs.params.get(InvokeExprType.signatureSymbol) match {
          case Some(InvokeExprType.Sig(selector, signature)) =>
            //val m = ztd.scope.getMethod(c.binding, "toString".toCharArray(), Array(), new FakeInvocationSite(TypeBinding.VOID))
            val scope = vs.owner.javaScope
            ZaluumCompletionEngineScala.findBySignature(cud,scope, c, selector, signature) match {
              case Some(p: ProblemMethodBinding) =>
                error("problem method " + p + p.problemId(), vs.decl)
              case Some(m) =>
                if (m.returnType != null && m.returnType != TypeBinding.VOID) {
                  val out = vs.portInstances find { _.name == Name("out") } getOrElse { vs.createOut(Name("out")).pi }
                  out.missing = false
                  out.finalTpe = cud.zaluumScope.getJavaType(m.returnType)
                }
                for ((p, i) <- m.parameters.zipWithIndex) {
                  val name = Name("p" + i)
                  val in = vs.portInstances find { _.name == name } getOrElse { vs.createIn(Name("p" + i)).pi }
                  in.missing = false
                  in.finalTpe = cud.zaluumScope.getJavaType(p);
                }
                vs.info = m
              // check connections
              case None =>
                error("method not found", vs.decl)
            }

          case _ => error("signature missing", vs.decl)
        }
      }
      def checkTypes() {
        bs.thisVal.portInstances foreach { api =>
          val pi = api.asInstanceOf[PortInstance]
          pi.missing = false
          pi.portSymbol match {
            case Some(p) => pi.finalTpe = p.tpe
            case None => error("Cannot find port " + api, bs.decl)
          }
        }
        for (vs <- bs.executionOrder) {
          vs.tpe match {
            case bs: BoxTypeSymbol => checkBoxTypes(vs)
            case b: BinExprType => checkBinExprTypes(vs)
            case LiteralExprType => checkLiteralExprType(vs)
            case e: UnaryExprType => checkUnaryExprType(vs)
            case InvokeExprType => checkInvokeExprType(vs)
          }
          checkGhostPorts(vs)
        }
        checkBoxTypes(bs.thisVal)
      }
      def storeTypesInConnectionsAndJunctions(c: Clump) {
        val outO = c.ports.find(p ⇒ !p.flowIn) map { _.pi }
        outO foreach { out =>
          for (con <- c.connections) { con.tpe = out.finalTpe }
          for (jun <- c.junctions) { jun.tpe = out.finalTpe }
        }
      }
    }
  }
}
object ZaluumCompletionEngineScala {
  def findBySignature(
    cud: ZaluumCompilationUnitDeclaration,
    zcs: ZaluumClassScope,
    c: ClassJavaType, selector: String, signature: String) = {

    allMethods(engineFor(cud), zcs, c) find { m =>
      m.selector.mkString == selector &&
        m.signature().mkString == signature
    }
  }
  
  def engineFor(cud:ZaluumCompilationUnitDeclaration) : ZaluumCompletionEngine= {
	  val lookup = cud.zaluumScope.environment
	  new ZaluumCompletionEngine(lookup)    
  }
  
  def engineForVs(vs: ValSymbol): ZaluumCompletionEngine =
    engineFor(vs.owner.javaScope.compilationUnitScope()
      .asInstanceOf[ZaluumCompilationUnitScope].cud)
 

  def allMethods(engine: ZaluumCompletionEngine, zcs: ZaluumClassScope, c: ClassJavaType): List[MethodBinding] = {
    val methodsFound = engine.findAllMethods(c.binding, zcs)

    var l = List[MethodBinding]()
    for (i <- 0 until methodsFound.size) {
      val o = methodsFound.elementAt(i).asInstanceOf[Array[_]]
      val method = o(0).asInstanceOf[MethodBinding]
      l ::= method
      val tpe = o(1).asInstanceOf[Object]
      //println("method " + method + " receiverType " + " " + tpe.getClass)
    }
    l
  }
}
class FakeInvocationSite(val expectedType: TypeBinding) extends InvocationSite {
  def genericTypeArguments() = null
  def isSuperAccess() = false
  def isTypeAccess() = false
  def setActualReceiverType(receiverType: ReferenceBinding) {}
  def setDepth(depth: Int) {}
  def setFieldIndex(depth: Int) {}
  def sourceEnd() = 0
  def sourceStart() = 0
}
