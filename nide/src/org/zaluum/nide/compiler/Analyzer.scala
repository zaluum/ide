package org.zaluum.nide.compiler

import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import scala.collection.mutable.Buffer

/*   def isValidJavaIdentifier(s: String) = {
    s != null &&
      s.length != 0 &&
      Character.isJavaIdentifierStart(s(0)) &&
      s.view(1, s.length).forall { Character.isJavaIdentifierPart(_) }
  }
  */

class CompilationException extends Exception
class Reporter {
  case class Error(msg: String, mark: Option[Location])
  val errors = Buffer[Error]()
  def report(str: String, mark: Option[Location] = None) {
    errors += Error(str, mark)
  }
  def check() {
    if (!errors.isEmpty)
      fail
  }
  def fail = throw new CompilationException()

  def fail(err: String, mark: Option[Location] = None): Nothing = {
    report(err)
    fail
  }
  def apply(assertion: Boolean, res: ⇒ String, mark: Option[Location] = None, fail: Boolean = false) {
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
  def toRelativePathClass = toRelativePath + ".class"
  def internal = str.replace('.', '/')
}
trait Scope {
  def lookupPort(name: Name): Option[Symbol]
  def lookupVal(name: Name): Option[Symbol]
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[Type]
  def lookupBoxTypeLocal(name: Name): Option[Type]
  def enter(sym: Symbol): Symbol
  def root: Symbol
}

class FakeGlobalScope(realGlobal: Scope) extends LocalScope(realGlobal) { // for presentation compiler
  case object fakeRoot extends Symbol {
    val owner = NoSymbol
    scope = FakeGlobalScope.this
    override val name = null
  }
  override val root = fakeRoot
}
class LocalScope(val enclosingScope: Scope) extends Scope with Namer {
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
  def lookupBoxTypeLocal(name: Name): Option[Type] = boxes.get(name)
  def enter(sym: Symbol): Symbol = {
    val entry = (sym.name -> sym)
    sym match {
      case p: PortSymbol ⇒ ports += entry
      case b: BoxTypeSymbol ⇒ boxes += (sym.name -> sym.asInstanceOf[Type])
      case v: ValSymbol ⇒ vals += entry
    }
    sym
  }
  def usedNames = (boxes.keySet.map { _.str } ++ vals.keySet.map { _.str } ++ ports.keySet.map { _.str }).toSet
  def root = enclosingScope.root
}
trait ReporterAdapter {
  def location(tree: Tree): Location
  def reporter: Reporter
  def error(str: String, tree: Tree) = reporter.report(str, Some(location(tree)))
}
class Analyzer(val reporter: Reporter, val toCompile: Tree, val global: Scope) {
  def globLocation(t: Tree) = Location(toCompile.pathOf(t).getOrElse(throw new Exception("error")))
  class Namer(initOwner: Symbol) extends Traverser(initOwner) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def defineBox(symbol: Symbol, tree: Tree): Symbol = {
      define(symbol, currentScope, currentScope.lookupBoxType(symbol.name).isDefined, tree)
    }
    def defineVal(symbol: Symbol, tree: Tree): Symbol =
      define(symbol, currentScope, currentScope.lookupVal(symbol.name).isDefined, tree)
    def definePort(symbol: Symbol, tree: Tree): Symbol = {
      define(symbol, currentScope, currentScope.lookupPort(symbol.name).isDefined, tree)
    }
    def define(symbol: Symbol, scope: Scope, dupl: Boolean, tree: Tree): Symbol = {
      if (dupl) error("Duplicate symbol " + symbol.name, tree)
      symbol.scope = scope
      tree.scope = scope
      tree.symbol = symbol
      symbol.decl = tree
      scope enter symbol
    }

    override def traverse(tree: Tree) {
      tree match {
        case BoxDef(name, image, defs, vals, ports, connections) ⇒
          // TODO inner class names $ if currentOwner is BoxTypeSymbol? 
          defineBox(new BoxTypeSymbol(currentOwner, name, image), tree)
        case p@PortDef(name, typeName, dir, inPos, extPos) ⇒
          definePort(new PortSymbol(currentOwner, name, extPos, dir), tree)
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
          defineVal(new ValSymbol(currentOwner, name), tree)
        case _ ⇒
          tree.scope = currentScope
      }
      super.traverse(tree)
    }
  }
  class Resolver(global: Symbol) extends Traverser(global) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    override def traverse(tree: Tree) {
      super.traverse(tree)
      tree match {
        case PortDef(name, typeName, in, inPos, extPos) ⇒
          tree.symbol.tpe = currentScope.lookupType(typeName) getOrElse {
            error("Port type not found " + typeName, tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
          tree.symbol.tpe = currentScope.lookupBoxType(typeName) getOrElse {
            error("Box class " + typeName + " not found", tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case ValRef(name) ⇒
          tree.symbol = currentScope.lookupVal(name) getOrElse {
            error("Box not found " + name, tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case PortRef(fromTree, name, in) ⇒ // TODO filter in?
          tree.symbol = fromTree.tpe match {
            case b: BoxTypeSymbol ⇒
              b.lookupPort(name).getOrElse {
                error("Port not found " + name, tree);
                NoSymbol
              }
            case tpe ⇒ NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case ThisRef ⇒
          tree.symbol = currentOwner // TODO what symbol for this?
          tree.tpe = currentOwner.asInstanceOf[BoxTypeSymbol]
        case _ ⇒
      }
    }
  }

  class CheckConnections(b: Tree, owner: Symbol) {
    val acyclic = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
    var usedInputs = Set[PortRef]()
    var connections = Buffer[(PortRef, PortRef)]()
    object Checker extends Traverser(owner) with ReporterAdapter with ConnectionHelper {
      def location(tree: Tree) = globLocation(tree)
      def reporter = Analyzer.this.reporter
      override def traverse(tree: Tree) {
        tree match {
          case b: BoxDef ⇒
            traverseTrees(b.vals)
            traverseTrees(b.connections)
            b.defs foreach {
              _ match {
                case bDef: BoxDef ⇒ new CheckConnections(bDef, b.symbol).check
              }
            }
          case c@ConnectionDef(a, b) ⇒
            if (a.symbol == NoSymbol || b.symbol == NoSymbol) {
              error("incomplete connection " + a + "<->" + b, tree)
            } else if (a.tpe != b.tpe) {
              error("connection " + a + "<->" + b + " is not type compatible", tree)
            } else {
              // check direction
              val connection = direction(c)
              connections += connection
              val (from, to) = connection
              // check only one connection per input
              if (usedInputs.contains(to))
                error("input already used " + to, tree)
              usedInputs += to
              // check graph consistency
              (from.fromRef, to.fromRef) match {
                case (va: ValDef, vb: ValDef) ⇒
                  try {
                    acyclic.addDagEdge(va.symbol.asInstanceOf[ValSymbol], vb.symbol.asInstanceOf[ValSymbol]);
                  } catch {
                    case e: CycleFoundException ⇒ error("cycle found ", tree)
                  }
                case _ ⇒
              }

              tree.tpe = a.tpe
            }
          case v: ValDef ⇒ acyclic.addVertex(v.symbol.asInstanceOf[ValSymbol])
          case _ ⇒
        }
      }
    }

    def check() {
      import scala.collection.JavaConversions._
      Checker.traverse(b)
      val topo = new TopologicalOrderIterator(acyclic);
      b.symbol.asInstanceOf[BoxTypeSymbol].executionOrder = topo.toList
    }
  }
  def compile(): Tree = {
    val root = global.root
    new Namer(root).traverse(toCompile)
    new Resolver(root).traverse(toCompile)
    new CheckConnections(toCompile, root).check()
    toCompile
  }
}
trait ConnectionHelper extends ReporterAdapter {
  implicit def reporter: Reporter
  def direction(c: ConnectionDef): (PortRef, PortRef) = {
    implicit val tree: Tree = c
    def isIn(ap: PortRef): Boolean = ap.symbol match {
      case s: PortSymbol ⇒
        s.dir match {
          case In ⇒ true
          case Out ⇒ false
          case Shift ⇒ ap.in
        }
      case _ ⇒ true
    }
    (c.a, c.b) match {
      case (ap@PortRef(av: ValRef, _, ain), bp@PortRef(bv: ValRef, _, bin)) ⇒
        (isIn(ap), isIn(bp)) match {
          case (true, false) ⇒ (bp, ap)
          case (false, true) ⇒ (ap, bp)
          case _ ⇒ error("invalid connection", c); (ap, bp)
        }
      case (ap@PortRef(av: ValRef, _, _), bp@PortRef(ThisRef, _, _)) ⇒
        (isIn(ap), isIn(bp)) match {
          case (true, true) ⇒ (bp, ap)
          case (false, false) ⇒ (ap, bp)
          case _ ⇒ error("invalid connection", c); (ap, bp)
        }
      case (ap@PortRef(ThisRef, _, _), bp@PortRef(bv: ValRef, _, _)) ⇒
        (isIn(ap), isIn(bp)) match {
          case (true, true) ⇒ (ap, bp)
          case (false, false) ⇒ (bp, ap)
          case _ ⇒ error("invalid connection", c); (ap, bp)
        }
      case (ap@PortRef(ThisRef, _, _), bp@PortRef(ThisRef, _, _)) ⇒
        error("invalid throught connection. TODO fix this", c); (ap, bp)
    }
  }
}