package org.zaluum.nide.compiler

import javax.swing.JPanel
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
  def classNameWithoutPackage = str.split('.').last
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
  def enter[S <: Symbol](sym: S): S
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
  def lookupPort(name: Name): Option[Symbol] = ports.get(name)
  def lookupVal(name: Name): Option[Symbol] = vals.get(name)
  def lookupType(name: Name): Option[Type] = enclosingScope.lookupType(name)
  def lookupBoxType(name: Name): Option[Type] =
    boxes.get(name) orElse { enclosingScope.lookupBoxType(name) }
  def lookupBoxTypeLocal(name: Name): Option[Type] = boxes.get(name)
  def enter[S <: Symbol](sym: S): S = {
    val entry = (sym.name -> sym)
    sym match {
      case p: IOSymbol ⇒ ports += entry
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
    def defineBox(symbol: BoxTypeSymbol, tree: Tree): BoxTypeSymbol =
      define(symbol, currentScope, currentScope.lookupBoxType(symbol.name).isDefined, tree)
    def defineVal(symbol: Symbol, tree: Tree): Symbol =
      define(symbol, currentScope, currentScope.lookupVal(symbol.name).isDefined, tree)
    def definePort(symbol: Symbol, tree: Tree): Symbol = {
      define(symbol, currentScope, currentScope.lookupPort(symbol.name).isDefined, tree)
    }
    def define[S <: Symbol](symbol: S, scope: Scope, dupl: Boolean, tree: Tree): S = {
      if (dupl) error("Duplicate symbol " + symbol.name, tree)
      symbol.scope = scope
      tree.scope = scope
      tree.symbol = symbol
      symbol.decl = tree
      scope enter symbol
    }

    override def traverse(tree: Tree) {
      tree match {
        case BoxDef(name, superName, image, defs, vals, ports, connections) ⇒
          val cl = Some(Name(classOf[JPanel].getName))
          val newSym = new BoxTypeSymbol(currentOwner, name, superName, image, cl)
          val sym = defineBox(newSym, tree)
          superName foreach { sn ⇒
            currentScope.lookupBoxType(sn) match {
              case Some(bs: BoxTypeSymbol) ⇒
                sym.superSymbol = Some(bs)
                println("found super " + bs + " for " + sym)
              case None ⇒
                error("Super box type not found " + sn, tree)
            }
          }
        case p@PortDef(name, typeName, dir, inPos, extPos) ⇒
          definePort(new PortSymbol(currentOwner.asInstanceOf[BoxTypeSymbol], name, extPos, dir), tree)
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize, params) ⇒
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
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize, params) ⇒
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
                error("Port not found " + name + " in box type " + b, tree);
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
  class CheckParams(global: Symbol) extends Traverser(global) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def parseValue(p: Param, parSymbol: ParamSymbol, valSymbol: ValSymbol) {
      p.symbol = parSymbol
      p.tpe = parSymbol.tpe
      try {
        parSymbol.tpe.name match {
          case Name("double") ⇒ valSymbol.params += (parSymbol -> p.value.toDouble)
          case n ⇒ error("error type " +n.str+ " cannot parse literals", p)
        }
      } catch {
        case e ⇒ error("invalid literal: " + p.value, p)
      }
    }
    override def traverse(tree: Tree) {
      super.traverse(tree)
      tree match {
        case p@Param(name, value) ⇒
          val valSym = currentOwner.asInstanceOf[ValSymbol]
          valSym.tpe match {
            case b: BoxTypeSymbol ⇒
              b.params.find(_.name == name) match {
                case Some(parSymbol) ⇒ parseValue(p, parSymbol, valSym)
                case None ⇒ error("Parameter " + name + " does not exist", p)
              }
            case _ ⇒ error("cannot find type of valDef owner " + p, p) // already failed
          }
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
          case c@ConnectionDef(a, b, waypoints) ⇒
            if (a.symbol == NoSymbol || b.symbol == NoSymbol) {
              error("incomplete connection " + a + "<->" + b, tree)
            } else if (a.tpe != b.tpe) {
              error("connection " + a.tpe + "<->" + b.tpe + " is not type compatible", tree)
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
                case (va: ValRef, vb: ValRef) ⇒
                  try {
                    println(va.symbol.name + "->" + vb.symbol.name)
                    acyclic.addDagEdge(va.symbol.asInstanceOf[ValSymbol], vb.symbol.asInstanceOf[ValSymbol]);
                  } catch {
                    case e: CycleFoundException ⇒ error("cycle found ", tree)
                  }
                case (ThisRef, b) ⇒
                case (a, ThisRef) ⇒
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
    new CheckParams(root).traverse(toCompile)
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
        (s.dir, ap.fromRef) match {
          case (In, v: ValRef) ⇒ true
          case (In, ThisRef) ⇒ false
          case (Out, v: ValRef) ⇒ false
          case (Out, ThisRef) ⇒ true
          case (Shift, v: ValRef) ⇒ ap.in
          case (Shift, ThisRef) ⇒ ap.in
        }
      case _ ⇒ true
    }
    (c.a, c.b) match {
      case (ap: PortRef, bp: PortRef) ⇒
        (isIn(ap), isIn(bp)) match {
          case (true, false) ⇒ (bp, ap)
          case (false, true) ⇒ (ap, bp)
          case _ ⇒ error("invalid connection. Must connect output and inputs.", c); (ap, bp)
        }
    }
  }
}