package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import org.jgrapht.experimental.dag.DirectedAcyclicGraph;
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.zaluum.nide.model._

class Compiled(val m: Model, val order: List[Box],
  val portType: Map[PortRef, TypedPort], val boxType: Map[Box, BoxClass], val source: String) {
  lazy val boxesInOrder = m.boxes.toList.sortWith(_.name < _.name)
  lazy val portDeclInOrder = m.portDecls.toList.sortWith(_.name < _.name)
}
object Compiler {
  def isValidJavaIdentifier(s: String) = {
    s != null &&
      s.length != 0 &&
      Character.isJavaIdentifierStart(s(0)) &&
      s.view(1, s.length).forall { Character.isJavaIdentifierPart(_) }
  }
  def partClassname(classname: String) = classname.split("[\\.]").toList;
  def isFullyQualifiedClassname(classname: String) = {
    def checkCharStart(c: Char) = Character.isJavaIdentifierStart(c) || Character.isIdentifierIgnorable(c)
    def checkCharPart(c: Char) = Character.isJavaIdentifierPart(c) || Character.isIdentifierIgnorable(c)
    def checkParts = {
      val parts = partClassname(classname)
      parts.length != 0 && parts.forall { part ⇒
        !part.isEmpty &&
          checkCharStart(part(0)) &&
          part.view(1, part.length).forall { checkCharPart(_) }
      }
    }
    classname != null && checkParts
  }
}
class Compiler(val m: Model, val boxClassPath: BoxClassPath) {
  object reporter {
    val errors = Buffer[String]()
    def report(str: String, mark: Option[AnyRef] = None) {
      errors += str
    }
    def check() {
      if (!errors.isEmpty)
        fail
    }
    def fail = throw new Exception("Compilation errors")

    def fail(err: String, mark: Option[AnyRef] = None): Nothing = {
      report(err)
      fail
    }
    def apply(assertion: Boolean, res: ⇒ String, mark: Option[AnyRef] = None, fail: Boolean = false) {
      if (!assertion) report(res) // TODO mark
      if (fail) check()
    }
    def apply() = check()
  }
  var boxTypes = Map[Box, BoxClass]()
  var portType = Map[PortRef, TypedPort]()
  def getBoxClass(cl: String): Option[BoxClass] =  boxClassPath.find(cl)
  def checkValidClassname(classname: String) = {
    // check characters
    reporter(Compiler.isFullyQualifiedClassname(classname), "classname " + classname + " is not a valid class name")
    // TODO check already defined

  }
  def checkModelPorts() {
    var names = Set[String]()
    for (portDecl ← m.portDecls) {
      val name = portDecl.name
      reporter(Compiler.isValidJavaIdentifier(name), name + " is not a valid Java identifier", Some(portDecl))
      reporter(!names(name), "Port name " + name + " is already defined", Some(portDecl))
      names = names + name
      portType += (ModelPortRef(name) -> TypedPort("double", portDecl.in, name, portDecl.pos))
    }
  }
  def checkPortRef(portRef: PortRef, blame: AnyRef) = {
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
          reporter.report("model port ref not found" , Some(blame))
      }
    }
  }
  def checkBoxes() {
    var names = (m.portDecls map {_.name} ).toSet
    for (b<-m.boxes) {
      reporter (!names(b.name),"Box name is repeated or the name of a declared port", Some(b))
      names = names + b.name
      boxTypes += (b -> checkBox(b))
    }
  }
  def checkBox(b: Box) = {
    // check name
    reporter(Compiler.isValidJavaIdentifier(b.name), b.name + " is not a valid Java identifier", Some(b))
    // check className is a box class
    val boxClass = getBoxClass(b.className).getOrElse { reporter.fail(b.className + " is not a valid box class", Some(b)) }
    // check box signature is compatible
    boxClass
  }
  def checkValidPort(port: PortRef) {

  }
  def checkConnections() = {
    val acyclic = new DirectedAcyclicGraph[Box, DefaultEdge](classOf[DefaultEdge])
    m.boxes foreach { acyclic.addVertex(_) }
    var portsUsed = Set[PortRef]()
    def isModelPort(p: PortRef) = p.isInstanceOf[ModelPortRef] 
    def checkConnection(c: Connection) {
      // complete connection
      reporter(c.from.isDefined && c.to.isDefined, "Connection c is incomplete", Some(c), true)
      val from = c.from.get
      val to = c.to.get
      checkPortRef(from,c)
      checkPortRef(to,c)
      reporter.check() // TODO fail better
      // auto connection
      reporter(from != to, "Connection to itself", Some(c))
      // port type and direction
      (portType(from), portType(to)) match {
        case (TypedPort(a, fromIn, _, _), TypedPort(b, toIn, _, _)) if (a == b) ⇒
          (fromIn, toIn, isModelPort(from), isModelPort(to)) match {
            case (true, true, true, false) ⇒ // in -> in model -> box
            case (false, true, false, false) ⇒ // out -> in box->box 
            case (false, false, false, true) ⇒ // out->out box->model
            case (true, false, true, true) ⇒ // in -> out model model
            case _ ⇒ reporter.report("Invalid connection")
          }
        case (a:TypedPort,b:TypedPort) ⇒ reporter.report("Incompatible port types " + a + " " + b, Some(c))
      }
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
    m.connections foreach { checkConnection(_) }
    reporter.check()
    // compute correct execution order
    val topo = new TopologicalOrderIterator[Box, DefaultEdge](acyclic);
    import scala.collection.JavaConversions._
    topo.toList
  }
  def compile() = {
    // check definition
    checkValidClassname(m.className)
    checkModelPorts()
    reporter.check()
    //checkValidPorts(m.ports)
    // check all boxes recursive
    checkBoxes()
    reporter.check()
    // check connections
    val order = checkConnections
    reporter.check()
    new Compiled(m, order, portType, boxTypes, "source.zaluum")
  }
}