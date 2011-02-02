package org.zaluum.nide.newcompiler

import java.io.StringWriter
import org.zaluum.nide.model.Point

abstract class Tree extends Product {
  //var pos : Position = NoPosition
  var tpe: Type = null
  var symbol: Symbol = null
  var scope: Scope = null
  def hasSymbol = false
  def isDef = false
  def isEmpty = false

  /** The direct child trees of this tree
   *  EmptyTrees are always omitted. Lists are collapsed.
   */
  def children: List[Tree] = {
    def subtrees(x: Any): List[Tree] = x match {
      case EmptyTree ⇒ List()
      case t: Tree ⇒ List(t)
      case xs: List[_] ⇒ xs flatMap subtrees
      case _ ⇒ List()
    }
    productIterator.toList flatMap subtrees
  }

  private[zaluum] def copyAttrs(tree: Tree): this.type = {
    //pos = tree.pos
    tpe = tree.tpe
    if (hasSymbol) symbol = tree.symbol
    this
  }

  override def hashCode(): Int = super.hashCode()

  override def equals(that: Any): Boolean = that match {
    case t: Tree ⇒ this eq t
    case _ ⇒ false
  }
}

trait SymTree extends Tree {
  override def hasSymbol = true
  symbol = NoSymbol
}
abstract class DefTree extends SymTree {
  def name: Name
  override def isDef = true
}
trait RefTree extends SymTree {
  def name: Name
}
trait TermTree extends Tree

trait TypTree extends Tree
//
class StrictTreeCopier {
  def BoxDef(tree: Tree, name: Name, defs: List[Tree], vals: List[Tree], ports: List[Tree], connections: List[Tree]) =
    new BoxDef(name, defs, vals, ports, connections).copyAttrs(tree)
  def PortDef(tree: Tree, name: Name, typeName: Name, in: Boolean, inPos: Point, extPos: Point) =
    new PortDef(name, typeName, in, inPos, extPos).copyAttrs(tree)
  def ValDef(tree: Tree, name: Name, typeName: Name) =
    new ValDef(name, typeName).copyAttrs(tree)
  def ConnectionDef(tree: Tree, a: Tree, b: Tree) =
    new ConnectionDef(a, b).copyAttrs(tree)
}
/*
      case EmptyTree =>
      case BoxDef(name, defs, vals, ports, connections) =>
      case PortDef(name, in, inPos, extPos) =>
      case ValDef(name,tpt)=>
      case ConnectionDef(a,b) =>
   */
// Transformer
abstract class Transformer {
  val treeCopy: StrictTreeCopier = new StrictTreeCopier
  protected var currentOwner: Symbol = NoSymbol; //definitions.RootClass
  def transform(tree: Tree): Tree = tree match {
    case EmptyTree ⇒
      tree
    case BoxDef(name, defs, vals, ports, connections) ⇒
      atOwner(tree.symbol) {
        treeCopy.BoxDef(tree, name,
          transformTrees(defs),
          transformTrees(vals),
          transformTrees(ports),
          transformTrees(connections))
      }
    case PortDef(name, typeName, in, inPos, extPos) ⇒
      treeCopy.PortDef(tree, name, typeName, in, inPos, extPos)
    case ValDef(name, typeName) ⇒
      treeCopy.ValDef(tree, name, typeName)
    case ConnectionDef(a, b) ⇒
      atOwner(tree.symbol) {
        treeCopy.ConnectionDef(tree, transform(a), transform(b))
      }
  }

  def transformTrees(trees: List[Tree]): List[Tree] =
    trees mapConserve (transform(_))

  def atOwner[A](owner: Symbol)(trans: ⇒ A): A = {
    val prevOwner = currentOwner
    currentOwner = owner
    val result = trans
    currentOwner = prevOwner
    result
  }
}
// Traverser
abstract class Traverser(initSymbol : Symbol) {
  protected var currentOwner: Symbol = initSymbol;
  protected var currentScope: Scope = initSymbol.scope;
  def error(str:String)(implicit tree:Tree) { println(str + " " + tree) }

  def traverse(tree: Tree): Unit = tree match {
    case EmptyTree ⇒
      ;
    case b@BoxDef(name, defs, vals, ports, connections) ⇒
      atOwner(tree.symbol) {
        traverseTrees(defs)
        traverseTrees(vals)
        traverseTrees(ports)
        traverseTrees(connections)
      }
    case ValDef(_, _) ⇒
    case ConnectionDef(a, b) ⇒
    case PortDef(_, _, _, _, _) ⇒

  }
  def traverseTrees(trees: List[Tree]) {
    trees foreach traverse
  }
  def atOwner(owner: Symbol)(traverse: ⇒ Unit) {
    val prevOwner = currentOwner
    val prevScope = currentScope
    currentOwner = owner
    currentScope = owner match {
      case s: Scope ⇒ s
      case _ ⇒ currentScope
    }
    traverse
    currentOwner = prevOwner
    currentScope = prevScope
  }
}
// ----- tree node alternatives --------------------------------------

/** The empty tree */
case object EmptyTree extends TermTree {
  override def isEmpty = true
}

/* Zaluum */
case class BoxDef(name: Name,
  defs: List[Tree],
  vals: List[Tree],
  ports: List[Tree],
  connections: List[Tree]) extends DefTree
case class PortDef(name: Name, typeName: Name, in: Boolean, inPos: Point, extPos: Point) extends DefTree
case class BoxRef(name: Name) extends RefTree
case class PortRef(name: Name, from: Tree) extends RefTree
case class ValDef(name: Name, typeName: Name) extends DefTree
case class ConnectionDef(a: Tree, b: Tree) extends Tree
