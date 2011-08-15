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
  def cud = analyzer.cud
  val bl = b.sym
  val template = bl.template
  val acyclic = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
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
          case v: ValDef ⇒ acyclic.addVertex(v.sym) // valdefs always have symbol
          case j @ Junction(name, _) ⇒
            bl.connections.lookupJunction(name) match {
              case Some(j) ⇒ error("junction name already exists", j)
              case None    ⇒ bl.connections.junctions += j
            }
          case c @ ConnectionDef(a, b, waypoints) ⇒
            if (a.isEmpty || b.isEmpty) {
              error("incomplete connection " + a + "<->" + b, tree)
            } else {
              bl.connections.addConnection(c)
            }
          case o ⇒ println("DEBUG: analyzerconnections other" + o)
        }
      }
    }
    connectionNamer.traverse(b)
  }
  // check 
  protected def check() {
    // 1 - Check clumps. Do not check for port existence.
    for (c ← bl.connections.clumps) { checkClump(c) }
    // 2 - compute execution order
    import scala.collection.JavaConversions._
    bl.executionOrder = new TopologicalOrderIterator(acyclic).toList
    // 3 - Propagate and check types
    checkTypes();
    // 4- Put calculated types to the clump
    for (c ← bl.connections.clumps) storeTypesInConnectionsAndJunctions(c)
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
          acyclic.addDagEdge(vout.valSymbol, vin.valSymbol);
        } catch {
          case e: CycleFoundException      ⇒ errorDag("Cycle found.")
          case e: IllegalArgumentException ⇒ errorDag("Loop connection found. Cannot connect a box to itself.")
        }
      }
    import org.zaluum.nide.RichCast._
      def isInside(p: PortInstance) = p.valSymbol == template.thisVal
    for (in ← ins) {
      if (!isInside(out) && !isInside(in))
        addDag(out, in)
    }
  }
  def assignBoxTypeSymbolTypes(vs:ValSymbol) {
    for (pi<-vs.portInstances; ps <- pi.portSymbol) {
      pi.missing = false
      pi.finalTpe = ps.tpe
    }
  }
  def checkBoxTypes(vs: ValSymbol) {
    checkGhostPorts(vs)
    checkPortConnectionsTypes(vs)
  }
  def checkGhostPorts(vs: ValSymbol) {
    for (pi ← vs.portInstances) {
      if (pi.missing) error("Ghost port. Cannot find port " + pi, vs.decl)
    }
  }
  def checkPortConnectionsTypes(vs: ValSymbol) {
    for (pi ← vs.portInstances) {
      if (pi.finalTpe == NoSymbol) error("Port type not found " + pi, b)
      else {
        for ((from, blame) ← bl.connections.connectedFrom.get(pi)) {
          if (!checkAssignmentPossible(from.finalTpe, pi.finalTpe)) {
            error("Connection with incompatible types", blame)
          }
        }
      }
    }
  }
  def checkAssignmentPossible(from: Type, to: Type): Boolean = {
    if (to == NoSymbol) return false
    from match {
      case NoSymbol ⇒ false
      case f: PrimitiveJavaType ⇒
        to match {
          case t: PrimitiveJavaType if t == f ⇒ true
          case t: PrimitiveJavaType ⇒
            t == f || primitives.widening(f, t)
          case t: ClassJavaType ⇒
            cud.zaluumScope.getBoxedType(f).binding.isCompatibleWith(t.binding)
          case _ ⇒ false
        }
      case f: ClassJavaType ⇒
        to match {
          case t: PrimitiveJavaType ⇒
            primitives.getUnboxedType(f) match {
              case Some(fp) ⇒ fp == t || primitives.widening(fp, t)
              case None     ⇒ false
            }
          case t: JavaType ⇒
            f.binding.isCompatibleWith(to.binding);
        }
      case f: JavaType ⇒
        f.binding.isCompatibleWith(to.binding); //array
      case _ ⇒ false
    }
  }
  def checkTypes() {
    if (main) {
      template.thisVal.portInstances foreach { api ⇒
        val pi = api.asInstanceOf[PortInstance]
        pi.missing = false
        pi.portSymbol match {
          case Some(ps) ⇒ pi.finalTpe = ps.tpe
          case None     ⇒ error("Cannot find port " + api, bl.decl)
        }
      }
    }
    val exprChecker = new ExpressionChecker(this)
    val objectChecker = new OOChecker(this)
    for (vs ← bl.executionOrder) {
      vs.tpe match {
        case bs: BoxTypeSymbol   ⇒ assignBoxTypeSymbolTypes(vs); checkBoxTypes(vs)
        case b: BinExprType      ⇒ exprChecker.checkBinExprTypes(vs)
        case LiteralExprType     ⇒ exprChecker.checkLiteralExprType(vs)
        case e: UnaryExprType    ⇒ exprChecker.checkUnaryExprType(vs)
        case t: ThisExprType     ⇒ objectChecker.checkThisExprType(vs); checkPortConnectionsTypes(vs)
        case t: StaticExprType   ⇒ objectChecker.checkStaticExprType(vs); checkPortConnectionsTypes(vs)
        case t: TemplateExprType ⇒ objectChecker.checkTemplateExprType(vs)
      }
      checkGhostPorts(vs)
    }
    checkBoxTypes(template.thisVal)
  }
  def storeTypesInConnectionsAndJunctions(c: Clump) {
    val outO = c.ports.find(p ⇒ !p.flowIn) map { _.pi }
    outO foreach { out ⇒
      for (con ← c.connections) { con.tpe = out.finalTpe }
      for (jun ← c.junctions) { jun.tpe = out.finalTpe }
    }
  }
}
trait CheckerPart extends ReporterAdapter {
  def c: CheckConnections
  def cud = c.cud
  def bl = c.bl
  def location(tree: Tree) = c.location(tree)
  def reporter = c.reporter
  def scope(vs: ValSymbol) : ZaluumClassScope = {
    vs.owner.template match {
      case b:BoxTypeSymbol => b.javaScope
      case own:ValSymbol => scope(own) 
    }
  }
  def connectedFrom(p: PortInstance) = bl.connections.connectedFrom.get(p)
  def fromTpe(p: PortInstance) = connectedFrom(p).map { _._1.finalTpe }.getOrElse(NoSymbol)
  def blame(p: PortInstance) = connectedFrom(p) map { _._2 }
  def unboxIfNeeded(t: Type) = t match {
    case p: ClassJavaType ⇒ primitives.getUnboxedType(p).getOrElse(t)
    case _                ⇒ t
  }
}