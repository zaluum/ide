package org.zaluum.nide.compiler

import scala.collection.immutable.Stack
import scala.collection.mutable.Buffer
import java.io.StringWriter

trait SelectionSubject
abstract class Tree extends Product with SelectionSubject {
  //var pos : Position = NoPosition
  var tpe: Type = NoSymbol
  var symbol: Symbol = NoSymbol
  var scope: Scope = null
  var line : Int = 0
  def hasSymbol = false
  def isDef = false
  def isEmpty = false
  private def findPath0(l:Int) : (Option[Tree],Int) = {
    if (l<=0) (None,0)
    else if (l==1) (Some(this),1)
    else {
      var visited = 1
      for (c<-children) {
        val (t, visits) = c.findPath0(l-visited)
        visited += visits
        if (t.isDefined) {
          return (t,visited)
        }
      }
      (None,visited)
    }
  }
  def findPath(l: Int): Option[Tree] = findPath0(l)._1
  def assignLine(l:Int) : Int = {
    this.line = l
    var x = l+1
    for (c<-children) {
      x = c.assignLine(x)
    }
    x
  }
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
    symbol = tree.symbol
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
abstract class EditTransformer extends CopyTransformer with MapTransformer

trait MapTransformer extends Transformer {
  var map = Map[SelectionSubject, SelectionSubject]()
  abstract override protected def transform(tree: Tree): Tree = {
    val transformed = super.transform(tree)
    map += (tree -> transformed)
    transformed
  }
}
trait CopySymbolTransformer extends Transformer {
  abstract override protected def transform(tree: Tree): Tree =
    super.transform(tree).copyAttrs(tree)
}
trait CopyTransformer extends Transformer {
  val defaultTransform: PartialFunction[Tree, Tree] = {
    case e@EmptyTree ⇒ e
    case b@BoxDef(name, superName, guiSize, image, defs, vals, ports, connections, junctions) ⇒
      atOwner(b.symbol) {
        BoxDef(name, superName, guiSize, image,
          transformTrees(defs),
          transformTrees(vals),
          transformTrees(ports),
          transformTrees(connections),
          transformTrees(junctions))
      }
    case PortDef(name, typeName, dir, inPos, extPos) ⇒
      PortDef(name, typeName, dir, inPos, extPos)
    case v:ValDef ⇒
      atOwner(v.symbol) {
        v.copy(params = transformTrees(v.params))
      }
    case p: Param ⇒ p.copy()
    case c@ConnectionDef(a, b, wp) ⇒
      atOwner(c.symbol) {
        ConnectionDef(transform(a), transform(b), wp)
      }
    case PortRef(from, name, in) ⇒
      PortRef(transform(from), name, in)
    case ValRef(name) ⇒ ValRef(name)
    case j: JunctionRef ⇒ j.copy()
    case j: Junction ⇒ j.copy()
    case t: ThisRef ⇒ ThisRef()
  }
}
abstract class Transformer extends OwnerHelper[Tree] {
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
abstract class Traverser(initSymbol: Symbol) extends OwnerHelper[Unit] {
  currentOwner = initSymbol
  currentScope = initSymbol.scope
  def traverse(tree: Tree): Unit = {
    tree match {
      case EmptyTree ⇒
        ;
      case b@BoxDef(name, superName, guiSize, image, defs, vals, ports, connections, junctions) ⇒
        atOwner(tree.symbol) {
          traverseTrees(defs)
          traverseTrees(vals)
          traverseTrees(ports)
          traverseTrees(connections)
          traverseTrees(junctions)
        }
      case v: ValDef ⇒
        atOwner(tree.symbol) {
          traverseTrees(v.params)
        }
      case p: Param ⇒
      case ConnectionDef(a, b, waypoints) ⇒
        traverse(a)
        traverse(b)
      case p: PortDef ⇒
      case PortRef(tree, _, _) ⇒
        traverse(tree)
      case j: Junction ⇒
      case j: JunctionRef ⇒
      case ValRef(_) ⇒
      case ThisRef() ⇒
    }
  }
  def traverseTrees(trees: List[Tree]) {
    trees foreach traverse
  }

}
object PrettyPrinter {
  def print(str: String, deep: Int) {
    println(new String(Array.fill(deep) { ' ' }) + str)
  }
  def print(trees: List[Tree], deep: Int) {
    trees.foreach { print(_, deep) }
  }
  def sym(tree: Tree) = " sym= " + tree.symbol + " tpe= " + tree.tpe
  def print(tree: Tree, deep: Int): Unit = tree match {
    case EmptyTree ⇒ print("EmptyTree", deep)
    case b@BoxDef(name, superName, guiSize, image, defs, vals, ports, connections, junctions) ⇒
      print("BoxDef(" + name + " extends " + superName + ", " + image, deep)
      print(guiSize.toString, deep + 1)
      print(defs, deep + 1)
      print(vals, deep + 1)
      print(ports, deep + 1)
      print(connections, deep + 1)
      print(junctions, deep + 1)
      print(")" + sym(b), deep)
    case v: ValDef ⇒
      print("ValDef(" + List(v.name, v.pos, v.size, v.typeName, v.guiPos, v.guiSize).mkString(","), deep)
      print("params: " + v.params, deep + 1)
      print("constructors:" + v.constructorParams.mkString(","), deep +1)
      print("constructorTypes:("+ v.constructorTypes.mkString(",") +")" , deep +1)
      print(")" + sym(v), deep)
    case p: Param ⇒
      print(p.toString + sym(p), deep)
    case c@ConnectionDef(a, b, wp) ⇒
      print("ConnectionDef(", deep)
      print(a, deep + 1)
      print(b, deep + 1)
      for (p ← wp) {
        print(p.toString, deep + 1)
      }
      print(")" + sym(c), deep)
    case p@PortDef(_, _, _, _, _) ⇒
      print(p.toString + sym(p), deep)
    case p@PortRef(tree, a, b) ⇒
      print("PortRef(", deep)
      print(tree, deep + 1)
      print(a + ", " + b, deep + 1)
      print(")" + sym(p), deep)
    case _ ⇒ print(tree.toString + sym(tree), deep)
  }
}
abstract class OwnerHelper[A] {
  protected var currentOwner: Symbol = null
  protected var currentScope: Scope = null

  def atOwner(owner: Symbol)(traverse: ⇒ A): A = {
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
case object EmptyTree extends Tree {
  override def isEmpty = true
}

/* Definition */
case class BoxDef(name: Name, superName: Option[Name],
  guiSize: Option[Dimension],
  image: Option[String],
  defs: List[Tree],
  vals: List[Tree],
  ports: List[Tree],
  connections: List[Tree],
  junctions: List[Tree]) extends DefTree
sealed trait PortDir
case object In extends PortDir
case object Out extends PortDir
case object Shift extends PortDir
case class PortDef(name: Name, typeName: Name, dir: PortDir, inPos: Point, extPos: Point) extends DefTree with Positionable {
  def pos = inPos
}
case class ValRef(name: Name) extends RefTree
case class ThisRef() extends SymTree
case class PortRef(fromRef: Tree, name: Name, in: Boolean) extends RefTree
case class Param(key: Name, value: String) extends Tree
case class LabelDesc(description:String, pos:Vector2)
case class ValDef(
  name: Name,
  typeName: Name,
  pos: Point,
  size: Option[Dimension],
  guiPos: Option[Point],
  guiSize: Option[Dimension],
  params: List[Tree], 
  constructorParams:List[String],
  constructorTypes:List[Name],
  label : Option[LabelDesc],
  labelGui : Option[LabelDesc]
  ) extends DefTree with Positionable {
  def localTypeDecl = tpe match {
    case NoSymbol ⇒ None
    case b: BoxTypeSymbol ⇒ if (b.isLocal) Some(b.decl.asInstanceOf[BoxDef]) else None
  }
}
//case class SizeDef(pos: Point, size: Dimension) extends Tree
case class ConnectionDef(a: Tree, b: Tree, points: List[Point]) extends SymTree {
  def valRef(t: Tree): Option[ValRef] = portRef(t) flatMap { p ⇒
    p.fromRef match {
      case v: ValRef ⇒ Some(v)
      case _ ⇒ None
    }
  }
  def portRef(t: Tree): Option[PortRef] = t match {
    case EmptyTree ⇒ None
    case p: PortRef ⇒ Some(p)
 }
 def headPoint = points.headOption.getOrElse(Point(0, 0))
 def lastPoint = points.lastOption.getOrElse(Point(0, 0))

}
case class Junction(name: Name, p: Point) extends DefTree
case class JunctionRef(name: Name) extends RefTree
