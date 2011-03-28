package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import javax.swing.JComponent
trait Symbol {
  def owner: Symbol
  def name: Name
  var decl: Tree = EmptyTree
  var tpe: Type = NoSymbol
  var scope: Scope = null
 // override def toString = "Symbol(" + (if (name != null) name.toString else "NoSymbol") + ")"
}
trait Type extends Symbol
case object NoSymbol extends Symbol with Type {
  val owner = NoSymbol
  val name = null
}
class JavaType(val owner: Symbol, val name: Name) extends Symbol with Type {
  scope = owner.scope
  def descriptor = "L" + name.internal + ";"
}
class PrimitiveJavaType(owner: Symbol, name: Name, override val descriptor: String) extends JavaType(owner, name)
class ClassJavaType(val owner: Symbol, val name: Name) extends Type {
  scope = owner.scope
}
object PortPath {
  def apply(p: PortRef): PortPath = PortPath(p.fromRef.symbol, p.symbol)
}
// from can be BoxTypeSymbol if it is "this" or ValSymbol
case class PortPath(from: Symbol, port: Symbol)
case class Clump(var junctions: Set[Junction], var ports: Set[PortPath], var connections: Set[ConnectionDef])
class BoxTypeSymbol(
  val owner: Symbol,
  val name: Name,
  val superName: Option[Name],
  val image: Option[String],
  val visualClass: Option[Name],
  val abstractCl: Boolean = false) extends LocalScope(owner.scope) with Symbol with Type {

  object connections extends Namer {
    var junctions = Set[Junction]()
    def usedNames = junctions map { _.name.str }
    var flow = Map[PortPath, Set[PortPath]]()
    var clumps = Buffer[Clump]()
    def clumpOf(c: ConnectionDef) = clumps find { _.connections.contains(c) }
    def clumpOf(p: PortPath) = clumps find { _.ports.contains(p) }
    def clumpOf(j: Junction) = clumps find { _.junctions.contains(j) }
    def addPort(j: Junction, a: PortRef, c: ConnectionDef) {
      val sym = PortPath(a)
      val newClump = merge(clumpOf(sym), clumpOf(j))
      newClump.connections += c
      newClump.ports += sym
      newClump.junctions += j
    }
    def addPorts(a: PortRef, b: PortRef, c: ConnectionDef) {
      val as = PortPath(a)
      val bs = PortPath(b)
      val newClump = merge(clumpOf(as), clumpOf(bs))
      newClump.connections += c
      newClump.ports ++= Set(as, bs)
    }
    def addJunctions(j1: Junction, j2: Junction, c: ConnectionDef) {
      val newClump = merge(clumpOf(j1), clumpOf(j2))
      newClump.connections += c
      newClump.junctions ++= Set(j1, j2)
    }
    def lookupJunction(n: Name) = { junctions.find { _.name == n } }
    def merge(a: Option[Clump], b: Option[Clump]): Clump = {
      (a, b) match {
        case (Some(c1), Some(c2)) ⇒
          if (c1 != c2) {
            clumps -= c2
            c1.ports ++= c2.ports
            c1.connections ++= c2.connections
            c1.junctions ++= c2.junctions
          }
          c1
        case (Some(c1), None) ⇒ c1
        case (None, Some(c2)) ⇒ c2
        case (None, None) ⇒
          val clump = Clump(Set(), Set(), Set())
          clumps += clump
          clump
      }
    }
    def addConnection(c: ConnectionDef) = {
      (c.a, c.b) match {
        case (p: PortRef, j: JunctionRef) ⇒ addPort(lookupJunction(j.name).getOrElse{throw new RuntimeException("cannot find junction"  + j.name)}, p, c)
        case (j: JunctionRef, p: PortRef) ⇒ addPort(lookupJunction(j.name).get, p, c)
        case (p1: PortRef, p2: PortRef) ⇒ addPorts(p1, p2, c)
        case (j1: JunctionRef, j2: JunctionRef) ⇒ addJunctions(lookupJunction(j1.name).get, lookupJunction(j2.name).get, c)
        // FIXME not connected EmptyTrees
        case _ ⇒
      }
    }
  }
  var superSymbol: Option[BoxTypeSymbol] = None
  var source: String = "" // TODO
  def valsInOrder = boxes.values.toList.sortWith(_.name.str < _.name.str).asInstanceOf[List[ValSymbol]]
  def IOInOrder = ports.values.toList.sortWith(_.name.str < _.name.str).asInstanceOf[List[IOSymbol]]
  def params = ports.values collect { case p: ParamSymbol ⇒ p }
  var executionOrder = List[ValSymbol]()
  def fqName: Name = owner match {
    case bown: BoxTypeSymbol ⇒ Name(bown.fqName.str + "$" + name.str)
    case _ ⇒ name
  }
  def isLocal = owner.isInstanceOf[BoxTypeSymbol]
  override def toString = "BoxTypeSymbol(" + name.str + ", super=" + superSymbol + ")"
  override def lookupPort(name: Name): Option[Symbol] =
    super.lookupPort(name) orElse (superSymbol flatMap { _.lookupPort(name) })

}

//class ConnectionSymbol(val owner:Symbol, val name:Name, val from:Tree, val to:Tree) extends Symbol 
// TODO make two classes one that has values from the declaring tree and the other directly from symbol
class IOSymbol(val owner: BoxTypeSymbol, val name: Name, val dir: PortDir) extends Symbol {
  def box = owner
}
class PortSymbol(owner: BoxTypeSymbol, name: Name, val extPos: Point, dir: PortDir) extends IOSymbol(owner, name, dir) {
  //override def toString = "PortSymbol(" + name + ")"
}
class ParamSymbol(owner: BoxTypeSymbol, name: Name, val default: String, dir: PortDir) extends IOSymbol(owner, name, dir) {
  override def toString = "ParamSymbol(" + name + ")"
}
class ValSymbol(val owner: Symbol, val name: Name) extends Symbol {
  var params = Map[ParamSymbol, Any]()
 // override def toString = "ValSymbol(" + name + ")"
}

