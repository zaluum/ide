package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import javax.swing.JComponent
trait Symbol {
  def owner: Symbol
  def name: Name
  var decl: Tree = EmptyTree
  var tpe: Type = NoSymbol
  var scope: Scope = null
  override def toString = "Symbol(" + (if (name != null) super.toString else "NoSymbol") + ")"
}
trait Type extends Symbol
case object NoSymbol extends Symbol with Type {
  val owner = NoSymbol
  val name = null
}
abstract class JavaType(val owner: Symbol) extends Symbol with Type {
  scope = owner.scope
  def name: Name
  def descriptor: String
  override def toString = "JavaType(" + name + "@" + hashCode + ")"
}
class PrimitiveJavaType(owner: Symbol, val name: Name, override val descriptor: String) extends JavaType(owner) {

}
class ArrayType(owner: Symbol, val of: JavaType, val dim: Int) extends JavaType(owner) {
  assert(!of.isInstanceOf[ArrayType])
  def descriptor = "["*dim + of.descriptor
  def name = Name(of.name.str + "[]"*dim)
  override def equals(that: Any) = {
    that match {
      case a: ArrayType ⇒ a.of == of && a.dim == dim && a.owner == owner
      case _ ⇒ false
    }
  }
  override def hashCode = {
    41 * (41 * (41 + owner.hashCode) + of.hashCode) + dim
  }
  override def toString = "ArrayType(" + of.toString + ", " + dim + ")"
}
class ClassJavaType(owner: Symbol, val name: Name) extends JavaType(owner) {
  scope = owner.scope
  def descriptor = "L" + name.internal + ";"

}
object PortKey {
  def create(p: PortRef): PortKey = p.fromRef match {
    case t: ThisRef ⇒ BoxPortKey(p.name, p.in)
    case v: ValRef ⇒ ValPortKey(v.name, p.name, p.in)
  }
}
// from can be BoxTypeSymbol if it is "this" or ValSymbol
sealed trait PortKey {
  def toRef: PortRef
  def resolve(b: BoxTypeSymbol): Option[PortKeySym]
}
case class BoxPortKey(port: Name, in: Boolean) extends PortKey {
  def toRef = PortRef(ThisRef(), port, in)
  def resolve(bs: BoxTypeSymbol) = bs.lookupPort(port) collect { case p: PortSymbol ⇒ BoxPortKeySym(bs, p) }
}
case class ValPortKey(from: Name, port: Name, in: Boolean) extends PortKey {
  def toRef = PortRef(ValRef(from), port, in)
  def resolve(bs: BoxTypeSymbol) = {
    bs.lookupVal(from) match {
      case Some(v: ValSymbol) ⇒
        v.tpe match {
          case b: BoxTypeSymbol ⇒ b.lookupPort(port) match {
            case Some(p: PortSymbol) ⇒ Some(ValPortKeySym(bs, v, p))
            case _ ⇒ None
          }
          case _ ⇒ None
        }
      case _ ⇒ None
    }
  }
}
sealed trait PortKeySym {
  def box: BoxTypeSymbol
  def port: PortSymbol
}
case class BoxPortKeySym(box: BoxTypeSymbol, port: PortSymbol) extends PortKeySym
case class ValPortKeySym(box: BoxTypeSymbol, valSym: ValSymbol, port: PortSymbol) extends PortKeySym
case class Clump(var junctions: Set[Junction], var ports: Set[PortKey], var connections: Set[ConnectionDef])
class BoxTypeSymbol(
  val owner: Symbol,
  val name: Name,
  val pkg: Name,
  val superName: Option[Name],
  val image: Option[String],
  var visualClass: Option[Name],
  val abstractCl: Boolean = false) extends LocalScope(owner.scope) with Symbol with Type {
  def declaredPorts = ports
  def portsWithSuper: Map[Name, Symbol] = ports ++ superSymbol.map { _.portsWithSuper }.getOrElse(Map())
  def declaredVals = vals
  object connections extends Namer {
    var junctions = Set[Junction]()
    def usedNames = junctions map { _.name.str }
    var flow = Map[PortKey, Set[PortKey]]()
    var clumps = Buffer[Clump]()
    def clumpOf(c: ConnectionDef) = clumps find { _.connections.contains(c) }
    def clumpOf(p: PortKey) = clumps find { _.ports.contains(p) }
    def clumpOf(j: Junction) = clumps find { _.junctions.contains(j) }
    def addPort(j: Junction, a: PortRef, c: ConnectionDef) {
      val pk = PortKey.create(a)
      val newClump = merge(clumpOf(pk), clumpOf(j))
      newClump.connections += c
      newClump.ports += pk
      newClump.junctions += j
    }
    def addPorts(a: PortRef, b: PortRef, c: ConnectionDef) {
      val (as, bs) = (PortKey.create(a), PortKey.create(b))
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
        case (p: PortRef, j: JunctionRef) ⇒ addPort(lookupJunction(j.name).getOrElse { throw new RuntimeException("cannot find junction" + j.name) }, p, c)
        case (j: JunctionRef, p: PortRef) ⇒ addPort(lookupJunction(j.name).get, p, c)
        case (p1: PortRef, p2: PortRef) ⇒ addPorts(p1, p2, c)
        case (j1: JunctionRef, j2: JunctionRef) ⇒ addJunctions(lookupJunction(j1.name).get, lookupJunction(j2.name).get, c)
        // FIXME not connected EmptyTrees
        case _ ⇒
      }
    }
  }
  var _superSymbol: Option[BoxTypeSymbol] = None
  def superSymbol = {
    _superSymbol match {
      case Some(s) ⇒ _superSymbol
      case None ⇒ superName match {
        case Some(sn) ⇒
          _superSymbol = scope.lookupBoxType(sn).asInstanceOf[Option[BoxTypeSymbol]]
          _superSymbol
        case None ⇒ None
      }
    }
  }
  var okOverride = false
  var constructors = List[Constructor]()
  var source: String = "" // TODO
  def valsInOrder = boxes.values.toList.sortWith(_.name.str < _.name.str).asInstanceOf[List[ValSymbol]]
  def IOInOrder = ports.values.toList.sortWith(_.name.str < _.name.str).asInstanceOf[List[IOSymbol]]
  def params = ports.values collect { case p: ParamSymbol ⇒ p }
  var executionOrder = List[ValSymbol]()
  def fqName: Name = owner match {
    case bown: BoxTypeSymbol ⇒ Name(bown.fqName.str + "$" + name.str)
    case _ ⇒ if (pkg!="") Name(pkg.str+"."+name.str) else name
  }
  def isLocal = owner.isInstanceOf[BoxTypeSymbol]
  // override def toString = "BoxTypeSymbol(" + name.str + ", super=" + superSymbol + ")"
  override def lookupPort(name: Name): Option[Symbol] =
    super.lookupPort(name) orElse (superSymbol flatMap { _.lookupPort(name) })
  tpe = this
}

//class ConnectionSymbol(val owner:Symbol, val name:Name, val from:Tree, val to:Tree) extends Symbol 
// TODO make two classes one that has values from the declaring tree and the other directly from symbol
class IOSymbol(val owner: BoxTypeSymbol, val name: Name, val dir: PortDir) extends Symbol {
  def box = owner
}
class PortSymbol(owner: BoxTypeSymbol, name: Name, val extPos: Point, dir: PortDir) extends IOSymbol(owner, name, dir) {
  //override def toString = "PortSymbol(" + name + ")"
}
class Constructor(owner: BoxTypeSymbol, val params: List[ParamSymbol]) {
  override def toString = {
    if (params.isEmpty) "<default>()" else
      params.map(p ⇒ p.name.str + " : " + p.tpe.name.str).mkString(", ")
  }
  def matchesSignature(sig: List[Type]) = sig == params.map { _.tpe }
  def signature = { // FIXME might not be a JavaType it could be a BoxTypeSymbol
    val pars = params map { p ⇒ p.tpe.asInstanceOf[JavaType].descriptor } mkString;
    "(" + pars + ")V"
  }
}

class ParamSymbol(owner: BoxTypeSymbol, name: Name) extends IOSymbol(owner, name, In) {
  override def toString = "ParamSymbol(" + name + ")"
}
class ValSymbol(val owner: Symbol, val name: Name) extends Symbol {
  var params = Map[ParamSymbol, Any]()
  var constructor: Option[Constructor] = None
  var constructorParams = List[(Any, Type)]()
  // override def toString = "ValSymbol(" + name + ")"
}

