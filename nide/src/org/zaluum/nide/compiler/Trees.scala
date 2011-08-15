package org.zaluum.nide.compiler

import scala.collection.immutable.Stack
import scala.collection.mutable.Buffer
import java.io.StringWriter

trait SelectionSubject
abstract class Tree extends Product with SelectionSubject {
  var tpe: Type = null
  var symbol: Symbol = null
  var line: Int = 0
  def hasSymbol = false
  def isDef = false
  def isEmpty = false
  private def findPath0(l: Int): (Option[Tree], Int) = {
    if (l <= 0) (None, 0)
    else if (l == 1) (Some(this), 1)
    else {
      var visited = 1
      for (c ← children) {
        val (t, visits) = c.findPath0(l - visited)
        visited += visits
        if (t.isDefined) {
          return (t, visited)
        }
      }
      (None, visited)
    }
  }
  def findPath(l: Int): Option[Tree] = findPath0(l)._1
  def assignLine(l: Int): Int = {
    this.line = l
    var x = l + 1
    for (c ← children) {
      x = c.assignLine(x)
    }
    x
  }
  def deepchildren: List[Tree] =
    for (c ← children; dc ← c :: c.deepchildren) yield dc

  def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case t: Tree      ⇒ List(t)
        case o: Option[_] ⇒ for (ot ← o.toList; st ← subtrees(ot)) yield st
        case xs: List[_]  ⇒ xs flatMap subtrees
        case _            ⇒ List()
      }
    productIterator.toList flatMap subtrees
  }
  private[zaluum] def copyAttrs(tree: Tree): this.type = {
    tpe = tree.tpe
    symbol = tree.symbol
    this
  }

  override def hashCode(): Int = super.hashCode()

  override def equals(that: Any): Boolean = that match {
    case t: Tree ⇒ this eq t
    case _       ⇒ false
  }
  def print(depth: Int): String = {
      def print(a: Any): String = {
        a match {
          case t: Tree    ⇒ t.print(depth + 1)
          case l: List[_] ⇒ l.map { print(_) }.mkString("\n")
          case _          ⇒ (" " * (depth + 1)) + a.toString
        }
      }
    val prods = productIterator.toList
    (" " * depth) + this.productPrefix + "(" +
      (if (prods.isEmpty) ")"
      else {
        "\n" + (for (e ← prods) yield {
          print(e)
        }).mkString("\n") +
          "\n" + (" " * depth) + ")"
      })
  }
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
  abstract override protected def transform[A <: Tree](tree: A): A = {
    val transformed = super.transform(tree)
    map += (tree -> transformed)
    transformed
  }
}
trait CopySymbolTransformer extends Transformer {
  abstract override protected def transform[A <: Tree](tree: A): A =
    super.transform(tree).copyAttrs(tree)
}
trait CopyTransformer extends Transformer {
  val defaultTransform: PartialFunction[Tree, Tree] = {
    case b: BoxDef ⇒
      atOwner(b.symbol) {
        b.copy(template = transform(b.template))
      }
    case t: Template ⇒
      t.copy(blocks = transformTrees(t.blocks),
        ports = transformTrees(t.ports))
    case b: Block ⇒
      b.copy(
        junctions = transformTrees(b.junctions),
        connections = transformTrees(b.connections),
        parameters = transformTrees(b.parameters),
        valDefs = transformTrees(b.valDefs))
    case PortDef(name, typeName, dir, inPos, extPos) ⇒
      PortDef(name, typeName, dir, inPos, extPos)
    case v: ValDef ⇒
      atOwner(v.symbol) {
        v.copy(params = transformTrees(v.params),
          template = transformOption(v.template))
      }
    case p: Param ⇒ p.copy()
    case c @ ConnectionDef(a, b, wp) ⇒
      atOwner(c.symbol) {
        ConnectionDef(transformOption(a), transformOption(b), wp)
      }
    case PortRef(from, name, in) ⇒
      PortRef(transform(from), name, in)
    case ValRef(name)   ⇒ ValRef(name)
    case j: JunctionRef ⇒ j.copy()
    case j: Junction    ⇒ j.copy()
    case t: ThisRef     ⇒ ThisRef()
  }
}
abstract class Transformer extends OwnerHelper[Tree] {
  protected val defaultTransform: PartialFunction[Tree, Tree]
  protected val trans: PartialFunction[Tree, Tree]
  protected lazy val finalTrans = trans.orElse(defaultTransform)
  def apply(tree: Tree, initOwner: Symbol = null): Tree = {
    currentOwner = initOwner
    transform(tree)
  }
  protected def transform[A <: Tree](tree: A): A = finalTrans.apply(tree).asInstanceOf[A]
  protected def transformOption[A <: Tree](tree: Option[A]): Option[A] =
    tree map (transform(_))
  protected def transformTrees[A <: Tree](trees: List[A]): List[A] =
    trees mapConserve (transform(_))
}
// Traverser
abstract class Traverser(initSymbol: Symbol) extends OwnerHelper[Unit] {
  currentOwner = initSymbol
  def traverse(tree: Tree): Unit = {
    tree match {
      case b: BoxDef ⇒
        atOwner(tree.symbol) {
          traverse(b.template)
        }
      case t: Template ⇒
        atOwner(tree.symbol) {
          traverseTrees(t.ports)
          traverseTrees(t.blocks)
        }
      case b: Block ⇒
        atOwner(tree.symbol) {
          traverseTrees(b.valDefs)
          traverseTrees(b.junctions)
          traverseTrees(b.connections)
          traverseTrees(b.parameters)
        }
      case v: ValDef ⇒
        atOwner(tree.symbol) {
          traverseTrees(v.params)
          traverseOption(v.template)
        }
      case p: Param ⇒
      case ConnectionDef(a, b, waypoints) ⇒
        traverseOption(a)
        traverseOption(b)
      case p: PortDef ⇒
      case PortRef(tree, _, _) ⇒
        traverse(tree)
      case j: Junction    ⇒
      case j: JunctionRef ⇒
      case ValRef(_)      ⇒
      case ThisRef()      ⇒
    }
  }
  def traverseTrees(trees: List[Tree]) {
    trees foreach traverse
  }
  def traverseOption(o: Option[Tree]) {
    o foreach traverse
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
    case b: BoxDef ⇒
      print("BoxDef(" + b.pkg + " " + b.name + ", " + b.image, deep)
      print(b.guiSize.toString, deep + 1)
      print(b.template, deep + 1)
      print(")" + sym(b), deep)
    case t: Template ⇒
      print("Template( ", deep)
      print(t.blocks, deep + 1)
      print(t.ports, deep + 1)
      print(")", deep)
    case b: Block ⇒
      print("Block( ", deep)
      print(b.valDefs, deep + 1)
      print(b.connections, deep + 1)
      print(b.junctions, deep + 1)
      print(b.parameters, deep + 1)
      print(")", deep)
    case v: ValDef ⇒
      print("ValDef(" + List(v.name, v.pos, v.size, v.typeName, v.guiPos, v.guiSize).mkString(","), deep)
      print("params: " + v.params, deep + 1)
      print("constructors:" + v.constructorParams.mkString(","), deep + 1)
      print("constructorTypes:(" + v.constructorTypes.mkString(",") + ")", deep + 1)
      print(v.template.toList, deep + 1)
      print(")" + sym(v), deep)
    case p: Param ⇒
      print(p.toString + sym(p), deep)
    case c @ ConnectionDef(a, b, wp) ⇒
      print("ConnectionDef(", deep)
      a foreach { print(_, deep + 1) }
      b foreach { print(_, deep + 1) }
      for (p ← wp) {
        print(p.toString, deep + 1)
      }
      print(")" + sym(c), deep)
    case p @ PortDef(_, _, _, _, _) ⇒
      print(p.toString + sym(p), deep)
    case p @ PortRef(tree, a, b) ⇒
      print("PortRef(", deep)
      print(tree, deep + 1)
      print(a + ", " + b, deep + 1)
      print(")" + sym(p), deep)
    case _ ⇒ print(tree.toString + sym(tree), deep)
  }
}
abstract class OwnerHelper[A] {
  protected var currentOwner: Symbol = null

  def atOwner(owner: Symbol)(traverse: ⇒ A): A = {
    val prevOwner = currentOwner
    currentOwner = owner
    val result = traverse
    currentOwner = prevOwner
    result
  }
}
// ----- tree node alternatives --------------------------------------

/* Definition */
object BoxDef {
  def emptyBox(name: String, pkg: String) = {
    val block = Block(junctions = List(),
      connections = List(),
      parameters = List(),
      valDefs = List())
    val template = Template(ports = List(), blocks = List(block), currentBlock = None)
    BoxDef(name = Name(name),
      pkg = Name(pkg),
      guiSize = Some(Dimension(250, 250)),
      image = None,
      template = template)
  }
}
case class BoxDef(name: Name, // simple name
                  pkg: Name,
                  guiSize: Option[Dimension],
                  image: Option[String],
                  template: Template) extends Tree {
  def sym = symbol.asInstanceOf[BoxTypeSymbol]
}
object Template {
  def emptyTemplate(blocks: Int) = {
    Template(List.fill(blocks) { Block.empty }, List(), None)
  }
}
case class Template(blocks: List[Block], ports: List[PortDef], currentBlock: Option[String]) extends Tree {
  def sym: TemplateSymbol = symbol.asInstanceOf[TemplateSymbol]
}
object Block {
  def empty = Block(List(), List(), List(), List())
}
case class Block(
    junctions: List[Junction],
    connections: List[ConnectionDef],
    parameters: List[Param],
    valDefs: List[ValDef]) extends Tree {
  def sym = symbol.asInstanceOf[BlockSymbol]
}
object PortDir {
  def fromStr(str: String) = str match {
    case In.str    ⇒ In
    case Out.str   ⇒ Out
    case Shift.str ⇒ Shift
  }
}
sealed abstract class PortDir(val str: String, val desc: String)
case object In extends PortDir("<in>", "Port In")
case object Out extends PortDir("<out>", "Port Out")
case object Shift extends PortDir("<shift>", "Port Shift")
case class PortDef(name: Name, typeName: Name, dir: PortDir, inPos: Point, extPos: Point) extends Tree with Positionable {
  def pos = inPos
  def sym = symbol.asInstanceOf[PortSymbol]
  def renamePort(str: String): MapTransformer = {
    val newName = Name(sym.box.asInstanceOf[BoxTypeSymbol].freshName(str))
    new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case p: PortDef if (p == PortDef.this) ⇒
          p.copy(name = newName)
        case pr: PortRef ⇒
          if (pr.symbol == sym && pr.symbol != NoSymbol) {
            pr.copy(fromRef = transform(pr.fromRef), newName, pr.in)
          } else pr
      }
    }
  }
}
case class ValRef(name: Name) extends Tree
case class ThisRef() extends Tree
trait ConnectionEnd extends Tree
case class PortRef(fromRef: Tree, name: Name, in: Boolean) extends ConnectionEnd {// in as flow or as PortDir?
 def sym : PortSide = symbol.asInstanceOf[PortSide] 
}
case class Param(key: Name, value: String) extends Tree
case class LabelDesc(description: String, pos: Vector2)
object ValDef {
  def emptyValDef(name: Name, tpeName: Name, dst: Point) =
    ValDef(name, tpeName, dst, None, None, None, List(), List(), List(), None, None, None)
}
case class ValDef(
    name: Name,
    typeName: Name,
    pos: Point,
    size: Option[Dimension],
    guiPos: Option[Point],
    guiSize: Option[Dimension],
    params: List[Tree],
    constructorParams: List[String],
    constructorTypes: List[Name],
    label: Option[LabelDesc],
    labelGui: Option[LabelDesc],
    template: Option[Template]) extends Tree with Positionable {
  def sym = symbol.asInstanceOf[ValSymbol]
  def addOrReplaceParam(param: Param) = new EditTransformer() {
    val trans: PartialFunction[Tree, Tree] = {
      case v: ValDef if v == ValDef.this ⇒
        val filtered = v.params.asInstanceOf[List[Param]].filterNot(_.key == param.key)
        v.copy(
          template = transformOption(v.template),
          params = param :: filtered)
    }
  }
  def editLabel(gui: Boolean, s: String) =
    new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case v: ValDef if (v == ValDef.this) ⇒
          val oldl = if (gui) v.labelGui else v.label
          val lDesc = LabelDesc(s, oldl map { _.pos } getOrElse { Vector2(0, 0) })
          if (gui)
            v.copy(labelGui = Some(lDesc))
          else
            v.copy(label = Some(lDesc))
      }
    }
}
case class ConnectionDef(
    a: Option[ConnectionEnd],
    b: Option[ConnectionEnd],
    points: List[Point]) extends Tree {
  def headPoint = points.headOption.getOrElse(Point(0, 0))
  def lastPoint = points.lastOption.getOrElse(Point(0, 0))

}
case class Junction(name: Name, p: Point) extends Tree
case class JunctionRef(name: Name) extends ConnectionEnd
