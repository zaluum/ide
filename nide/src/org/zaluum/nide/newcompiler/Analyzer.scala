package org.zaluum.nide.newcompiler
import org.zaluum.nide.compiler.BoxClassPath
case class Name(str: String)
trait Scope {
  def lookupPort(name: Name): Option[Symbol]
  def lookupVal(name: Name): Option[Symbol]
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[Type]
  def enter(sym: Symbol): Symbol
}
class LocalScope(val enclosingScope: Option[Scope]) {
  private var ports = Map[Name, Symbol]()
  private var vals = Map[Name, Symbol]()
  private var boxes = Map[Name, Type]()
  def lookupPort(name: Name): Option[Symbol] =
    ports.get(name) orElse { enclosingScope flatMap { _.lookupPort(name) } }
  def lookupVal(name: Name): Option[Symbol] =
    vals.get(name) orElse { enclosingScope flatMap { _.lookupVal(name) } }
  def lookupType(name: Name): Option[Type] = enclosingScope flatMap { _.lookupType(name) }
  def lookupBoxType(name: Name): Option[Type] =
    boxes.get(name) orElse { enclosingScope flatMap { _.lookupBoxType(name) } }
  def enter(sym: Symbol): Symbol = {
    val entry = (sym.name -> sym)
    sym match {
      case p: PortSymbol ⇒ ports += entry
      case b: BoxTypeSymbol ⇒ boxes += (sym.name -> sym.asInstanceOf[Type])
      case v: ValSymbol ⇒ vals += entry
    }
    sym
  }
}

class Namer(initOwner: Symbol) extends Traverser(initOwner) {
  def defineBox(symbol: Symbol)(implicit tree: Tree): Symbol =
    define(symbol, currentScope, currentScope.lookupBoxType(symbol.name).isDefined)
  def defineVal(symbol: Symbol)(implicit tree: Tree): Symbol =
    define(symbol, currentScope, currentScope.lookupVal(symbol.name).isDefined)
  def definePort(symbol: Symbol)(implicit tree: Tree): Symbol =
    define(symbol, currentScope, currentScope.lookupPort(symbol.name).isDefined)

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
        defineBox(new BoxTypeSymbol(currentOwner, name))
      case p@PortDef(name, typeName, in, inPos, extPos) ⇒
        definePort(new PortSymbol(currentOwner, name))
      case v@ValDef(name, typeName) ⇒
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
      case v@ValDef(name, typeName) ⇒
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
          case EmptyTree ⇒
            currentScope.lookupPort(name) getOrElse {
              error("Port not found " + name); NoSymbol
            }
          case b ⇒
            b.scope.lookupPort(name) getOrElse {
              error("Port not found  " + name + " in box " + b); NoSymbol
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

class Compiler() {

}