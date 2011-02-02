package org.zaluum.nide.newcompiler

import org.zaluum.nide.compiler.Reporter
import org.zaluum.nide.compiler.BoxClassPath
import org.zaluum.nide.eclipse.EclipseBoxClasspath

case class Name(str: String) {
  //TODO
  def classNameWithoutPackage =Some(str) // TODO
  def toRelativePath : String = str.replace('.', '/')
  
}
trait Scope {
  def lookupPort(name: Name): Option[Symbol]
  def lookupVal(name: Name): Option[Symbol]
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[Type]
  def enter(sym: Symbol): Symbol
}
class LocalScope(val enclosingScope: Option[Scope]) extends Scope{
  var ports = Map[Name, Symbol]()
  var vals = Map[Name, Symbol]()
  var boxes = Map[Name, Type]()
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
class Analyzer(val reporter: Reporter, val toCompile:Tree, val global:EclipseBoxClasspath) {
  def error(str:String)(implicit tree:Tree) { println(str + " " + tree) }

  class Namer(initOwner: Symbol) extends Traverser(initOwner) {
    def defineBox(symbol: Symbol)(implicit tree: Tree): Symbol = {
      define(symbol, currentScope, currentScope.lookupBoxType(symbol.name).isDefined)
    }
    def defineVal(symbol: Symbol)(implicit tree: Tree): Symbol =
      define(symbol, currentScope, currentScope.lookupVal(symbol.name).isDefined)
    def definePort(symbol: Symbol)(implicit tree: Tree): Symbol ={
      println ("defining port symbol " +  symbol + " " + tree )
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
          definePort(new PortSymbol(currentOwner, name))
        case v@ValDef(name, typeName,_) ⇒
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
        case v@ValDef(name, typeName,_) ⇒
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
                error("Model port not found " + name); NoSymbol
              }
            case BoxRef(boxName) ⇒
             val res : Option[Symbol] = currentScope.lookupVal(boxName) flatMap  {
                _.tpe match {
                  case bt : BoxTypeSymbol => 
                    println("box found " + boxName + " ports " + bt.ports);
                    bt.lookupPort(name)
                  case _ => 
                    println("box not found " + boxName);None
                }
               } 
            res.getOrElse {
                error("Box port not found  " + name + " in box " + boxName); NoSymbol
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
  def compile() : Tree = {
    val root = global.RootSymbol
    new Namer(root).traverse(toCompile)
    new Resolver(root).traverse(toCompile)
    new Checker(root).traverse(toCompile)
    println (toCompile)
    toCompile
  }
}
