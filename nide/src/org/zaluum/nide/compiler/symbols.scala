package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import javax.swing.JComponent
trait Symbol {
  def owner: Symbol
  def name: Name
  var decl: Tree = EmptyTree
  var tpe: Type = NoSymbol
  var scope: Scope = null
  override def toString = "Symbol(" + (if (name != null) name.str else "NoSymbol") + ")"
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
  override def toString = "JavaType(" + name + ")"
}
class PrimitiveJavaType(owner: Symbol, val name: Name, override val descriptor: String) extends JavaType(owner) {

}
class ArrayType(owner: Symbol, val of: JavaType, val dim: Int) extends JavaType(owner) {
  assert(!of.isInstanceOf[ArrayType])
  def descriptor = "[" * dim + of.descriptor
  def name = Name(of.name.str + "[]" * dim)
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
case class Clump(var junctions: Set[Junction], var ports: Set[PortSide], var connections: Set[ConnectionDef])
trait BoxType extends Symbol with Type {
  protected def ports: Map[Name, Symbol]
  def declaredPorts = ports
  def portsWithSuper = ports
  def lookupPort(name: Name): Option[Symbol]
}
class BoxTypeSymbol(
  val owner: Symbol,
  val simpleName: Name, //Class name without package
  val pkg: Name, // pkgdecl
  val superName: Option[Name], //fqname
  val image: Option[String],
  var visualClass: Option[Name],
  val abstractCl: Boolean = false) extends LocalScope(owner.scope) with BoxType {
  var thisVal : ValSymbol = _
  var hasApply = false
  override def portsWithSuper: Map[Name, Symbol] = ports ++ superSymbol.map { _.portsWithSuper }.getOrElse(Map())
  def declaredVals = vals
  def name = if (pkg.str != "") Name(pkg.str + "." + simpleName.str) else simpleName // TODO this is not a full name for inner classes!
  def fqName: Name = owner match { // this is
    case bown: BoxTypeSymbol ⇒ Name(bown.fqName.str + "$" + simpleName.str)
    case _ ⇒ name
  }
  object connections extends Namer {
    var junctions = Set[Junction]()
    def usedNames = junctions map { _.name.str }
    var flow = Map[PortSide, Set[PortSide]]()
    var clumps = Buffer[Clump]()
    def clumpOf(c: ConnectionDef) = clumps find { _.connections.contains(c) }
    def clumpOf(p: PortSide) = clumps find { _.ports.contains(p) }
    def clumpOf(j: Junction) = clumps find { _.junctions.contains(j) }
    def addPort(j: Junction, a: PortRef, c: ConnectionDef) {
      val pk = PortSide.find(a,BoxTypeSymbol.this).get
      val newClump = merge(clumpOf(pk), clumpOf(j))
      newClump.connections += c
      newClump.ports += pk
      newClump.junctions += j
    }
    def addPorts(a: PortRef, b: PortRef, c: ConnectionDef) {
      val (as, bs) = (PortSide.find(a,BoxTypeSymbol.this).get, PortSide.find(b,BoxTypeSymbol.this).get)
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
  def valsInOrder = vals.values.toList.sortWith(_.name.str < _.name.str).asInstanceOf[List[ValSymbol]]
  def IOInOrder = ports.values.toList.sortWith(_.name.str < _.name.str).asInstanceOf[List[IOSymbol]]
  def params = ports.values collect { case p: ParamSymbol ⇒ p }
  var executionOrder = List[ValSymbol]()

  def isLocal = owner.isInstanceOf[BoxTypeSymbol]
  // override def toString = "BoxTypeSymbol(" + name.str + ", super=" + superSymbol + ")"
  override def lookupPort(name: Name): Option[Symbol] =
    super.lookupPort(name) orElse (superSymbol flatMap { _.lookupPort(name) })
  tpe = this
}
class SumExprType(val owner: Symbol) extends BoxType {
  val name = Name("Sum")
  val a = new PortSymbol(this, Name("a"), Point(0, 0), In)
  val b = new PortSymbol(this, Name("b"), Point(0, 0), In)
  val c = new PortSymbol(this, Name("c"), Point(0, 0), Out)
  val ports = List(a, b, c) map { a => (a.name -> a) } toMap
  def lookupPort(a: Name) = ports.get(a)

}
//class ConnectionSymbol(val owner:Symbol, val name:Name, val from:Tree, val to:Tree) extends Symbol 
// TODO make two classes one that has values from the declaring tree and the other directly from symbol
class IOSymbol(val owner: BoxType, val name: Name, val dir: PortDir) extends Symbol {
  def box = owner
}
class PortSymbol(owner: BoxType, name: Name, val extPos: Point, dir: PortDir) extends IOSymbol(owner, name, dir) {
  override def toString = "PortSymbol(" + name + ")"
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
class PortInstance(val portSymbol:PortSymbol,val valSymbol:ValSymbol) {
  def name = portSymbol.name
  override def toString = "PortInstance("+portSymbol+", "+valSymbol+")"
}
object PortSide {
  def find(p:PortRef, bs:BoxTypeSymbol) = {
    p.fromRef match {
    	case t: ThisRef ⇒ bs.thisVal.findPortSide(p)
    	case v: ValRef ⇒ for (vs <- bs.lookupVal(v.name); ps <- vs.asInstanceOf[ValSymbol].findPortSide(p)) yield ps
    }
  }
}
class PortSide(val pi:PortInstance, val in:Boolean, val fromInside:Boolean) {
  def isIn = if (fromInside) !in else in
  def name = pi.name
  def toRef = {
    if (fromInside) PortRef(ThisRef(),name,in) // ok?
    else PortRef(ValRef(pi.valSymbol.name), name,in)
  }
  override def toString() = "PortSide(" + pi.toString + ", in="+in+", fromInside="+fromInside+")"
}
class ValSymbol(val owner: Symbol, val name: Name) extends Symbol {
  var params = Map[ParamSymbol, Any]()
  var constructor: Option[Constructor] = None
  var constructorParams = List[(Any, Type)]()
  var portInstances = List[PortInstance]()
  var portSides = List[PortSide]()
  def findPortSide(pr:PortRef) = 
    portSides.find(ps => ps.pi.portSymbol.name==pr.name && ps.in== pr.in)
  def findPortSide(p:PortSymbol) = 
    portSides.find(ps => ps.pi.portSymbol==p)
    
  override def toString = "ValSymbol(" + name + ")"
}

