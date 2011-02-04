package org.zaluum.nide.newcompiler

import org.zaluum.nide.model.Namer
import scala.collection.mutable.Buffer
import org.zaluum.nide.model.Locatable
import org.zaluum.nide.model.Location
import org.zaluum.nide.eclipse.EclipseBoxClasspath
/*
   def isValidJavaIdentifier(s: String) = {
    s != null &&
      s.length != 0 &&
      Character.isJavaIdentifierStart(s(0)) &&
      s.view(1, s.length).forall { Character.isJavaIdentifierPart(_) }
  }
  lazy val boxesInOrder = bcd.boxes.toList.sortWith(_.name < _.name)
  lazy val portDeclInOrder = bcd.portDecls.toList.sortWith(_.name < _.name)
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
  }*/

class CompilationException extends Exception
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

case class Name(str: String) {
  //TODO
  def classNameWithoutPackage = Some(str) // TODO
  def toRelativePath: String = str.replace('.', '/')
  def internal = str.replace('.','/')
  def descriptor = "L" + internal + ";"

}
trait Scope {
  def lookupPort(name: Name): Option[Symbol]
  def lookupVal(name: Name): Option[Symbol]
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[Type]
  def enter(sym: Symbol): Symbol
  def root : Symbol
}

class FakeGlobalScope(realGlobal:Scope) extends LocalScope(realGlobal) { // for presentation compiler
  case object fakeRoot extends Symbol {
    val owner = NoSymbol
    scope = FakeGlobalScope.this
    override val name = null
  }
  override val root = fakeRoot
}
class LocalScope(val enclosingScope: Scope) extends Scope with Namer{
  var ports = Map[Name, Symbol]()
  var vals = Map[Name, Symbol]()
  var boxes = Map[Name, Type]()
  var connections = Set[ConnectionSymbol]()
  def lookupPort(name: Name): Option[Symbol] =
    ports.get(name) orElse { enclosingScope.lookupPort(name) } 
  def lookupVal(name: Name): Option[Symbol] =
    vals.get(name) orElse { enclosingScope.lookupVal(name) } 
  def lookupType(name: Name): Option[Type] = enclosingScope.lookupType(name) 
  def lookupBoxType(name: Name): Option[Type] =
    boxes.get(name) orElse { enclosingScope.lookupBoxType(name) } 
  def enter(sym: Symbol): Symbol = {
    val entry = (sym.name -> sym)
    sym match {
      case p: PortSymbol ⇒ ports += entry
      case b: BoxTypeSymbol ⇒ boxes += (sym.name -> sym.asInstanceOf[Type])
      case v: ValSymbol ⇒ vals += entry
    }
    sym
  }
  def usedNames = (vals.keySet.map {_.str} ++ ports.keySet.map{_.str}).toSet
  def root = enclosingScope.root
}
class Analyzer(val reporter: Reporter, val toCompile: Tree, val global: Scope) {
  def error(str: String)(implicit tree: Tree) { println(str + " " + tree) }

  class Namer(initOwner: Symbol) extends Traverser(initOwner) {
    def defineBox(symbol: Symbol)(implicit tree: Tree): Symbol = {
      define(symbol, currentScope, currentScope.lookupBoxType(symbol.name).isDefined)
    }
    def defineVal(symbol: Symbol)(implicit tree: Tree): Symbol =
      define(symbol, currentScope, currentScope.lookupVal(symbol.name).isDefined)
    def definePort(symbol: Symbol)(implicit tree: Tree): Symbol = {
      println("defining port symbol " + symbol + " " + tree)
      define(symbol, currentScope, currentScope.lookupPort(symbol.name).isDefined)
    }
    def define(symbol: Symbol, scope: Scope, dupl: Boolean)(implicit tree: Tree): Symbol = {
      if (dupl) error("Duplicate symbol " + symbol.name)
      symbol.scope = scope
      tree.scope = scope
      tree.symbol = symbol
      symbol.decl = tree
      scope enter symbol
    }

    override def traverse(tree1: Tree) {
      implicit val tree = tree1
      tree match {
        case BoxDef(name, defs, vals, ports, connections) ⇒
          // TODO inner class names $ if currentOwner is BoxTypeSymbol? 
          defineBox(new BoxTypeSymbol(currentOwner, name))
        case p@PortDef(name, typeName, in, inPos, extPos) ⇒
          definePort(new PortSymbol(currentOwner, name,extPos,in))
        case v@ValDef(name, typeName, pos, guiTree) ⇒
          defineVal(new ValSymbol(currentOwner, name))
        case _ ⇒
          tree.scope = currentScope
      }
      super.traverse(tree)
    }
  }
  class Resolver(global: Symbol) extends Traverser(global) {
    override def traverse(tree1: Tree) {
      implicit val tree = tree1
      super.traverse(tree)
      tree match {
        case PortDef(name, typeName, in, inPos, extPos) ⇒
          tree.symbol.tpe = currentScope.lookupType(typeName) getOrElse {
            error("Port type not found " + typeName); NoSymbol
          }
        case v@ValDef(name, typeName, pos, guiSize) ⇒
          tree.symbol.tpe = currentScope.lookupBoxType(typeName) getOrElse {
            error("Box class " + typeName + " not found"); NoSymbol
          }
        case BoxRef(name) ⇒
          tree.symbol = currentScope.lookupVal(name) getOrElse {
            error("Box not found " + name); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case PortRef(name, box) ⇒
          tree.symbol = box match {
            case BoxRef(boxName) ⇒
              val res: Option[Symbol] = currentScope.lookupVal(boxName) flatMap {
                _.tpe match {
                  case bt: BoxTypeSymbol ⇒
                    println("box found " + boxName + " ports " + bt.ports);
                    bt.lookupPort(name)
                  case _ ⇒
                    println("box not found " + boxName); None
                }
              }
              res.getOrElse {
                error("Box port not found  " + name + " in box " + boxName); NoSymbol
              }
            case _ ⇒
              currentScope.lookupPort(name) getOrElse {
                error("Model port not found " + name); NoSymbol
              }

          }
          tree.tpe = tree.symbol.tpe
        case _ ⇒
      }

    }
  }
  class Checker(global: Symbol) extends Traverser(global) {
    override def traverse(tree1: Tree) {
      implicit val tree = tree1
      tree match {
        case ConnectionDef(a, b) ⇒
          if (a.symbol == NoSymbol || b.symbol == NoSymbol)
            error("incomplete connection " + a + "<->" + b)
          if (a.tpe != b.tpe) error("connection " + a + "<->" + b + " is not type compatible")
        case _ ⇒
      }
      super.traverse(tree)
    }
  }
  def compile(): Tree = {
    val root = global.root
    new Namer(root).traverse(toCompile)
    new Resolver(root).traverse(toCompile)
    new Checker(root).traverse(toCompile)
    println(toCompile)
    toCompile
  }
}
