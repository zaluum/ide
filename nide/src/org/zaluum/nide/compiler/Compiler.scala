package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import org.jgrapht.experimental.dag.DirectedAcyclicGraph;
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.SimpleDirectedGraph;
import org.jgrapht.traverse.TopologicalOrderIterator;
import org.zaluum.nide.model._
class Compiled(val m: Model, val order: List[Box], val types: Map[Port, TypedPort], val source:String) {
  lazy val boxesInOrder = m.boxes.toList.sortWith(_.name < _.name);
}
object Compiler {
  def isValidJavaIdentifier(s: String) = {
      s!=null && 
      s.length !=0 && 
      Character.isJavaIdentifierStart(s(0)) &&
      s.view(1, s.length).forall { Character.isJavaIdentifierPart(_) }
  }
  def partClassname(classname:String) = classname.split("[\\.]").toList;    
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

  var portType = Map[Port, TypedPort]()
  def getBoxClass(cl: String): Option[BoxClass] = {
    boxClassPath.find(cl) 
  }
  def checkValidClassname(classname: String) = {
    // check characters
    reporter(Compiler.isFullyQualifiedClassname(classname),"classname " + classname + " is not a valid class name")
    // TODO check already defined
    
  } 
  def checkSignature(b: Box, bc: BoxClass) = {
    // check ports
    var ok = true
    for (p <- b.ports) {
      bc.ports find { _.name == p.name } match {
        case Some(tp) => portType += (p -> tp)
        case None => ok = false
      }
    }
    ok
    // TODO check parameters?
  }
  def checkBox(b: Box) {
    // check name
    reporter(Compiler.isValidJavaIdentifier(b.name), b.name + " is not a valid Java identifier", Some(b))
    // check className is a box class
    val boxClass = getBoxClass(b.className).getOrElse { reporter.fail(b.className + " is not a valid box class", Some(b)) }
    // check box signature is compatible
    checkSignature(b, boxClass)
  }
  def checkValidPort(port: Port) {

  }
  def checkConnections() = {
    val acyclic = new DirectedAcyclicGraph[Box, DefaultEdge](classOf[DefaultEdge])
    m.boxes foreach { acyclic.addVertex(_) }
    var portsUsed = Set[Port]()
    def isModelPort(p: Port) = false //TODO
    def checkConnection(c: Connection) {
      // complete connection
      reporter(c.from.isDefined && c.to.isDefined, "Connection c is incomplete", Some(c), true)
      val from = c.from.get
      val to = c.to.get
      // auto connection
      reporter(from != to, "Connection to itself", Some(c))
      // port type and direction
      (portType(from), portType(to)) match {
        case (TypedPort(a, fromIn, _), TypedPort(b, toIn, _)) if (a == b) ⇒
          (fromIn, toIn, isModelPort(from), isModelPort(to)) match {
            case (true, true, true, false) ⇒ // in -> in model -> box
            case (false, true, false, false) ⇒ // out -> in box->box 
            case (false, false, false, true) ⇒ // out->out box->model
            case (true, false, true, true) ⇒ // in -> out model model
            case _ ⇒ reporter.report("Invalid connection")
          }
        case _ ⇒ reporter.report("Incompatible port types", Some(c))
      }
      // multiple connections to one input
      reporter(!portsUsed.contains(to), "Double connection to port " + to, Some(to), true)
      portsUsed += to // this check won't work with nested boxes
      // check graph cycle
      try {
        if (!isModelPort(from) && !isModelPort(to))
          acyclic.addDagEdge(from.box, to.box)
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
    //checkValidPorts(m.ports)
    // check all boxes recursive
    m.boxes foreach { checkBox(_) }
    // check connections
    val order = checkConnections
    reporter.check()
    new Compiled(m, order, portType,"source.zaluum")
  }
}