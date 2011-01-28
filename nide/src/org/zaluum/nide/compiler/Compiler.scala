package org.zaluum.nide.compiler

import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.traverse.TopologicalOrderIterator
import org.zaluum.nide.model._
import scala.collection.mutable.Buffer

case class Compiled(val bcd: BoxClassDecl, val innerCompiled:Set[Compiled],  val order: List[Box],
  val portType: Map[PortRef, TypedPort], val boxType: Map[Box, BoxClass], val source: String) {
  lazy val boxesInOrder = bcd.boxes.toList.sortWith(_.name < _.name)
  lazy val portDeclInOrder = bcd.portDecls.toList.sortWith(_.name < _.name)
}
object Compiler {
  def isValidJavaIdentifier(s: String) = {
    s != null &&
      s.length != 0 &&
      Character.isJavaIdentifierStart(s(0)) &&
      s.view(1, s.length).forall { Character.isJavaIdentifierPart(_) }
  }
}
class CompilationException() extends Exception
class Reporter {
    case class Error(msg: String, mark: Option[Location])
    val errors = Buffer[Error]()
    def report(str: String, mark: Option[Locatable] = None) {
      errors += Error(str, mark map { _.location })
    }
    def check() {
      if (!errors.isEmpty)
        fail
    }
    def fail = throw new CompilationException()

    def fail(err: String, mark: Option[Locatable] = None): Nothing = {
      report(err)
      fail
    }
    def apply(assertion: Boolean, res: ⇒ String, mark: Option[Locatable] = None, fail: Boolean = false) {
      if (!assertion) report(res) // TODO mark
      if (fail) check()
    }
    def apply() = check()
    override def toString = errors.toString
  }
class Compiler(val bcd: BoxClassDecl, val boxClassPath: BoxClassPath, val reporter:Reporter) {
  
  var boxTypes = Map[Box, BoxClass]()
  var portType = Map[PortRef, TypedPort]()
  def getBoxClass(cl: BoxClassName): Option[BoxClass] = boxClassPath.find(cl) 

  def checkModelPorts() {
    var names = Set[String]()
    for (portDecl ← bcd.portDecls) {
      val name = portDecl.name
      reporter(Compiler.isValidJavaIdentifier(name), name + " is not a valid Java identifier", Some(portDecl))
      reporter(!names(name), "Port name " + name + " is already defined", Some(portDecl))
      names = names + name
      portType += (ModelPortRef(name) -> TypedPort(portDecl.descriptor, portDecl.in, name, portDecl.pos))
    }
  }
  def checkPortRef(portRef: PortRef, blame: Locatable) = {
    portType.get(portRef) getOrElse {
      portRef match {
        case b: BoxPortRef ⇒
          boxTypes.get(b.box) match {
            case Some(typ) ⇒
              typ.ports find { _.name == b.name } match {
                case Some(typedPort) ⇒ portType += (portRef -> typedPort);
                case None ⇒ reporter.report("port ref port not found", Some(blame));
              }
            case None ⇒ reporter.report("port ref box not found", Some(blame));
          }
        case _: ModelPortRef ⇒
          reporter.report("model port ref not found", Some(blame))
      }
    }
  }
  def checkBoxes() {
    var names = (bcd.portDecls map { _.name }).toSet
    for (b ← bcd.boxes) {
      reporter(!names(b.name), "Box name is repeated or the name of a declared port", Some(b))
      names = names + b.name
      boxTypes += (b -> checkBox(b))
    }
  }
  def checkBox(b: Box) = {
    // check name
    reporter(Compiler.isValidJavaIdentifier(b.name), b.name + " is not a valid Java identifier", Some(b))
    // check className is a box class
    val boxClass = getBoxClass(b.boxClassName).getOrElse { reporter.fail(b.boxClassName + " is not a valid box class", Some(b)) }
    // check box signature is compatible
    boxClass
  }
  def checkValidPort(port: PortRef) {

  }

  def checkConnections() = {
    val acyclic = new DirectedAcyclicGraph[Box, DefaultEdge](classOf[DefaultEdge])
    bcd.boxes foreach { acyclic.addVertex(_) }
    var portsUsed = Set[PortRef]()
    def isModelPort(p: PortRef) = p.isInstanceOf[ModelPortRef]
    def checkConnection(c: Connection) {
      // complete connection
      reporter(c.from.isDefined && c.to.isDefined, "Connection c is incomplete", Some(c), true)
      reporter.check()
      val p1 = c.from.get
      val p2 = c.to.get
      checkPortRef(p1, c)
      checkPortRef(p2, c)
      reporter.check() // TODO fail better
      // auto connection
      reporter(p1 != p2, "Connection to itself", Some(c))
      // port type and direction
      val dir: Option[(PortRef, PortRef)] = (portType(p1), portType(p2)) match {
        case (TypedPort(a, p1In, _, _), TypedPort(b, p2In, _, _)) if (a == b) ⇒
          c.connectionFlow(portType) orElse { reporter.report("Invalid connection", Some(c)); None }
        case (a: TypedPort, b: TypedPort) ⇒ reporter.report("Incompatible port types " + a + " " + b, Some(c)); None
      }
      reporter.check()
      val (from, to) = dir.get
      // multiple connections to one input
      reporter(!portsUsed.contains(to), "Double connection to port " + to, Some(to), true)
      portsUsed += to // this check won't work with nested boxes
      // check graph cycle
      try {
        (from, to) match {
          case (f: BoxPortRef, t: BoxPortRef) ⇒
            acyclic.addDagEdge(f.box, t.box)
          case _ ⇒
        }
      } catch {
        case e: CycleFoundException ⇒ reporter.fail("Cycle detected", Some(c))
      }
    }
    bcd.connections foreach { checkConnection(_) }
    reporter.check()
    // compute correct execution order
    val topo = new TopologicalOrderIterator[Box, DefaultEdge](acyclic);
    import scala.collection.JavaConversions._
    topo.toList
  }
  def checkValidClassname(className:BoxClassName) {
    reporter(className.isFullyQualifiedClassname, "class name " + className + " is not a valid class name")
    // TODO check already defined
  }
  def compileInnerClasses():Set[Compiled] = {
    for (inner <- bcd.innerClassDecls) yield{
      val compiler = new Compiler(inner, boxClassPath, reporter)
      reporter.check()
      compiler.compile()
    }
  }
  def compile() = {
    // check definition
    checkValidClassname(bcd.className)
    checkModelPorts()
    val innerCompiled = compileInnerClasses()
    reporter.check()
    //checkValidPorts(m.ports)
    // check all boxes recursive
    checkBoxes()
    reporter.check()
    // check connections
    val order = checkConnections
    reporter.check()
    Compiled(bcd, innerCompiled, order, portType, boxTypes, "source.zaluum")
  }
}