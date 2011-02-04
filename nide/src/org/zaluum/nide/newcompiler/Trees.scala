package org.zaluum.nide.newcompiler

import org.zaluum.nide.model.Dimension
import java.io.StringWriter
import org.zaluum.nide.model.{ Point, Positionable }

sealed abstract class Tree extends Product {
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
/*
    case EmptyTree ⇒
    case BoxDef(name, defs, vals, ports, connections) ⇒
    case PortDef(name, typeName, in, inPos, extPos) ⇒
    case ValDef(name, typeName,pos,guiSize) ⇒
    case ConnectionDef(a, b) ⇒
    case PortRef(name, from) =>
    case BoxRef(name) =>
   */
// Transformer
abstract class CopySymbolTransformer extends CopyTransformer {
  override protected def transform(tree: Tree): Tree = 
    super.transform(tree).copyAttrs(tree)
}
abstract class CopyTransformer extends Transformer {
  val defaultTransform:PartialFunction[Tree,Tree] = {  
    case e@EmptyTree ⇒ e
    case b@BoxDef(name, defs, vals, ports, connections) ⇒
      atOwner(b.symbol) {
        BoxDef(name,
          transformTrees(defs),
          transformTrees(vals),
          transformTrees(ports),
          transformTrees(connections))
      }
    case PortDef(name, typeName, in, inPos, extPos) ⇒
      PortDef(name, typeName, in, inPos, extPos)
    case ValDef(name, typeName, pos, guiSize) ⇒
      ValDef(name, typeName, pos, transform(guiSize))
    case c@ConnectionDef(a, b) ⇒
      atOwner(c.symbol) {
        ConnectionDef(transform(a), transform(b))
      }
    case PortRef(name, from) ⇒
      PortRef(name, transform(from))
    case BoxRef(name) ⇒
      BoxRef(name)
    case s@SizeDef(pos, size) ⇒
      s.copy()
  }
}
abstract class Transformer extends OwnerHelper[Tree]{
  protected val defaultTransform: PartialFunction[Tree, Tree]
  protected val trans: PartialFunction[Tree, Tree]
  protected lazy val finalTrans = trans.orElse(defaultTransform)
  def apply(tree: Tree, initOwner: Symbol = NoSymbol): Tree = {
    currentOwner = initOwner
    currentScope = initOwner.scope 
    transform(tree)
  }
  protected def transform(tree: Tree): Tree = finalTrans.apply(tree)
  protected def transformTrees(trees: List[Tree]): List[Tree] =
    trees mapConserve (transform(_))
}
// Traverser
abstract class Traverser(initSymbol: Symbol) extends OwnerHelper[Unit]{
  currentOwner = initSymbol
  currentScope = initSymbol.scope
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
    case ValDef(_, _, _, guiSize) ⇒
      traverse(guiSize)
    case ConnectionDef(a, b) ⇒
      atOwner(tree.symbol) {
        traverse(a)
        traverse(b)
      }
    case PortDef(_, _, _, _, _) ⇒
    case PortRef(_, tree) ⇒
      traverse(tree)
    case BoxRef(_) ⇒
    case SizeDef(_, _) ⇒
  }
  def traverseTrees(trees: List[Tree]) {
    trees foreach traverse
  }
  
}
abstract class OwnerHelper[A] {
  protected var currentOwner: Symbol = null
  protected var currentScope: Scope = null
  
  def atOwner(owner: Symbol)(traverse: ⇒ A) : A = {
    val prevOwner = currentOwner
    val prevScope = currentScope
    currentOwner = owner
    currentScope = owner match {
      case s: Scope ⇒ s
      case _ ⇒ currentScope
    }
    val result = traverse
    currentOwner = prevOwner
    currentScope = prevScope
    result
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
case class PortDef(name: Name, typeName: Name, in: Boolean, inPos: Point, extPos: Point) extends DefTree with Positionable {
  def pos = inPos
}
case class BoxRef(name: Name) extends RefTree
case class PortRef(name: Name, from: Tree) extends RefTree
case class ValDef(name: Name, typeName: Name, pos: Point, guiSize: Tree) extends DefTree with Positionable
case class SizeDef(pos: Point, size: Dimension) extends Tree
case class ConnectionDef(a: Tree, b: Tree) extends Tree
