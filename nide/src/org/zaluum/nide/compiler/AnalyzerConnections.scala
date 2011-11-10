package org.zaluum.nide.compiler
import scala.collection.JavaConversions.asScalaIterator
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.traverse.TopologicalOrderIterator
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope

class CheckConnections(b: Block, main: Boolean, val analyzer: Analyzer) extends ReporterAdapter {
  def location(tree: Tree) = analyzer.globLocation(tree)
  def reporter = analyzer.reporter
  def ztd = analyzer.ztd
  val bl: BlockSymbol = b.sym
  val template = bl.template
  var usedInputs = Set[PortInstance]()
  def run() = {
    val connectionNamer = new Traverser(null) {
      // parse connections
      override def traverse(tree: Tree) {
        tree match {
          case b: Block ⇒
            traverseTrees(b.valDefs)
            traverseTrees(b.junctions)
            traverseTrees(b.connections)
            check()
          case v: ValDef ⇒ bl.dag.addVertex(v.sym) // valdefs always have a symbol
          case j @ Junction(name, _) ⇒
            bl.connections.lookupJunction(name) match {
              case Some(j) ⇒ error("Fatal: junction " + j + " already exists", j)
              case None    ⇒ bl.connections.junctions += j
            }
          case c @ ConnectionDef(a, b, waypoints) ⇒
            if (a.isEmpty || b.isEmpty) {
              error("Incomplete connection", tree)
            }
            bl.connections.addConnection(c)
          case o ⇒ println("DEBUG: analyzerconnections other" + o)
        }
      }
    }
    connectionNamer.traverse(b)
  }
  // check
  // Analizer has created portinstances for known PortSymbols
  // Connections.addConnection has created portinstances with no PortSymbol 
  // for special checking
  protected def check() {
    // 1 - Check clumps. Do not check for port existence.
    for (c ← bl.connections.clumps) { checkFlowAndLoops(c) }
    // 2 - compute execution order
    import scala.collection.JavaConversions._
    bl.executionOrder = new TopologicalOrderIterator(bl.dag).toList.filter { _.isExecutable }
    // 3 - Check types for each valDef
    checkTypes();
    // 4- Put calculated types to the clump
    for (c ← bl.connections.clumps) storeTypesInConnectionsAndJunctions(c)
  }
  def errorConnection(str: String, c: ConnectionDef) {
    error(str, c)
    bl.connections.markAsBad(c)
  }
  def checkFlowAndLoops(c: Clump) {
    val ins = c.ports.filter(p ⇒ p.flowIn) map { _.pi }
    val outs = c.ports.filter(p ⇒ !p.flowIn) map { _.pi }
    if (outs.size == 0) errorConnection("Connection has no output attached", c.connections.head)
    else if (outs.size > 1) errorConnection("Connection with more than one output attached", c.connections.head)
    else if (ins.size == 0) errorConnection("Connection has no inputs attached", c.connections.head)
    else if (!usedInputs.intersect(ins).isEmpty) errorConnection("Input can only have one connection", c.connections.head) // TODO check online to identify offending connection 
    else {
      checkGraphFlow(c, ins, outs.head)
      // store flow
      usedInputs ++= ins
      for (in ← ins) {
        val r = (outs.head, c.findConnectionFor(in).get)
        bl.connections.connectedFrom += (in -> r)
      }
    }
  }
  def checkGraphFlow(c: Clump, ins: Set[PortInstance], out: PortInstance) {
    // check graph consistency
    bl.connections.flow += (out -> ins)
      def addDag(vout: PortInstance, vin: PortInstance) {
          def errorDag(str: String) {
            error(str, c.findConnectionFor(vin) orElse (c.findConnectionFor(vout))
              getOrElse (c.connections.head))
          }
        try {
          bl.dag.addDagEdge(vout.valSymbol, vin.valSymbol);
        } catch {
          case e: CycleFoundException      ⇒ errorDag("Cycle found. Data flow cannot have loops.")
          case e: IllegalArgumentException ⇒ errorDag("Loop connection found. Cannot connect a box to itself.")
        }
      }
    import org.zaluum.nide.utils.RichCast._
      def isInside(p: PortInstance) = p.valSymbol == template.thisVal
    for (in ← ins) {
      if (!isInside(out) && !isInside(in))
        addDag(out, in)
    }
  }

  def checkPortConnectionsTypes(vs: ValSymbol) {
    for (pi ← vs.portInstances) {
      val tpeName = pi.declOption.map { _.typeName.str }.getOrElse("")
      val blame: Tree = pi.declOption.getOrElse(vs.decl)
      if (pi.tpe.isEmpty && pi.portSymbol.isDefined)
        pi.tpe = pi.portSymbol.get.tpe
      bl.connections.connectedFrom.get(pi) match {
        case Some((from, blame)) ⇒
          if (pi.tpe == None && tpeName == "") {
            // do type inference
            pi.tpe = from.tpe
          } else if (pi.tpe == None) {
            bl.connections.markAsBad(blame)
          } else if (!checkAssignmentPossible(from.tpe, pi.tpe)) {
            errorConnection("Connection with incompatible types: " + from.tpeHumanStr + " to " + pi.tpeHumanStr, blame)
          }
        case _ ⇒
      }
      if (pi.missing)
        error("Port " + pi.name.str + " is missing", blame)
      else if (pi.tpe.isEmpty)
        error("Invalid port type " + tpeName, blame)
    }
  }
  def checkAssignmentPossible(from: Option[JavaType], to: Option[JavaType]): Boolean = {
    if (to.isEmpty) return false
    from match {
      case None ⇒ false
      case Some(f: PrimitiveJavaType) ⇒
        to match {
          case Some(t: PrimitiveJavaType) if t == f ⇒ true
          case Some(t: PrimitiveJavaType) ⇒
            t == f || primitives.widening(f, t)
          case Some(t: ClassJavaType) ⇒
            ztd.zaluumScope.getBoxedType(f).get.binding.isCompatibleWith(t.binding)
          case _ ⇒ false
        }
      case Some(f: ClassJavaType) ⇒
        to match {
          case Some(t: PrimitiveJavaType) ⇒
            primitives.getUnboxedType(f) match {
              case Some(fp) ⇒ fp == t || primitives.widening(fp, t)
              case None     ⇒ false
            }
          case Some(t: JavaType) ⇒
            f.binding.isCompatibleWith(t.binding);
          case _ ⇒ false
        }
      case Some(f: JavaType) ⇒
        f.binding.isCompatibleWith(to.get.binding); //array
      case _ ⇒ false
    }
  }
  def checkTypes() {
    if (main) {
      template.thisVal.portInstances foreach { pi ⇒
        assert(!pi.missing)
        pi.portSymbol match {
          case Some(ps) ⇒ pi.tpe = ps.tpe
          case None     ⇒ error("Cannot find port " + pi.name.str, b)
        }
      }
    }
    val exprChecker = new ExpressionChecker(this)
    val objectChecker = new OOChecker(this)
    for (vs ← bl.executionOrder) { // skip non executable
      vs.tpe foreach {
        _ match {
          case b: BinExprType      ⇒ exprChecker.checkBinExprTypes(vs)
          case LiteralExprType     ⇒ exprChecker.checkLiteralExprType(vs)
          case e: UnaryExprType    ⇒ exprChecker.checkUnaryExprType(vs)
          case t: ThisExprType     ⇒ objectChecker.checkThisExprType(vs)
          case ThisRefExprType     ⇒ objectChecker.checkThisRefExprType(vs)
          case t: StaticExprType   ⇒ objectChecker.checkStaticExprType(vs)
          case t: TemplateExprType ⇒ objectChecker.checkTemplateExprType(vs)
        }
      }
      checkPortConnectionsTypes(vs)
      //checkGhostPorts(vs)
    }
    checkPortConnectionsTypes(template.thisVal)
  }
  def storeTypesInConnectionsAndJunctions(c: Clump) {
    val outO = c.ports.find(p ⇒ !p.flowIn) map { _.pi }
    outO foreach { out ⇒
      for (con ← c.connections) { con.tpe = out.tpe }
      for (jun ← c.junctions) { jun.tpe = out.tpe }
    }
  }
}
trait CheckerPart extends ReporterAdapter {
  def c: CheckConnections
  def ztd = c.ztd
  def bl = c.bl
  def location(tree: Tree) = c.location(tree)
  def reporter = c.reporter
  def scope(vs: ValSymbol): ZaluumClassScope = {
    vs.owner.template match {
      case b: BoxSymbol   ⇒ b.scope
      case own: ValSymbol ⇒ scope(own)
    }
  }
  def connectedFrom(p: PortInstance) = bl.connections.connectedFrom.get(p)
  def fromTpe(p: PortInstance) = connectedFrom(p).flatMap { _._1.tpe }
  def blame(p: PortInstance) = connectedFrom(p) map { _._2 }
  def unboxIfNeeded(o: Option[JavaType]) = o flatMap {
    _ match {
      case p: ClassJavaType ⇒ primitives.getUnboxedType(p)
      case _                ⇒ o
    }
  }
}