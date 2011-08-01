package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import javax.swing.JComponent
import org.eclipse.jdt.internal.compiler.lookup.Binding
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
trait Symbol {
  def owner: Symbol
  def name: Name
  var decl: Tree = null
  var tpe: Type = NoSymbol
  override def toString = "Symbol(" + (if (name != null) name.str else "NoSymbol") + ")"
}
trait Type extends Symbol { // merge with javatype
  def fqName: Name
  def javaSize = 1
  type B <: TypeBinding
  var binding: B = _
}
case object NoSymbol extends Symbol with JavaType {
  val owner = NoSymbol
  def descriptor = null
  val name = Name("NoSymbol")
  def fqName = name
}
trait JavaType extends Type {
  val owner: Symbol
  def name: Name
  def descriptor: String
  override def toString = "JavaType(" + name + ")"
}
class PrimitiveJavaType(
  val name: Name,
  override val descriptor: String,
  override val javaSize: Int,
  val boxedName: Name,
  val boxMethod: String) extends JavaType {
  type B = BaseTypeBinding
  val owner = null
  def fqName = name
}
class ArrayType(val owner: Symbol, val of: JavaType, val dim: Int) extends JavaType {
  type B = ArrayBinding
  assert(!of.isInstanceOf[ArrayType])
  def descriptor = "[" * dim + of.descriptor
  def name = Name(of.name.str + "[]" * dim)
  def fqName = name
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
class ClassJavaType(val owner: Symbol, val fqName: Name) extends JavaType {
  type B = ReferenceBinding
  def descriptor = "L" + name.internal + ";"
  def name = fqName
}
case class Clump(var junctions: Set[Junction], var ports: Set[PortSide], var connections: Set[ConnectionDef], bl: BlockSymbol) {
  def findConnectionFor(pi: PortInstance) = {
    connections.find { con =>
      def isEnd(tree: Option[ConnectionEnd]) = {
        tree match {
          case Some(p: PortRef) =>
            PortSide.find(p, bl).exists(_.pi == pi)
          case _ => false
        }
      }
      isEnd(con.a) || isEnd(con.b)
    }
  }
}
trait TemplateSymbol extends Symbol {
  var ports = Map[Name, PortSymbol]()
  var blocks = List[BlockSymbol]()
  def lookupParam(name: Name): Option[ParamSymbol]
  var thisVal: ValSymbol = _ // should be template
}
trait BoxType extends TemplateSymbol with Type {
  def portsWithSuper = ports
  def lookupPortWithSuper(name:Name) : Option[PortSymbol]
}
class BlockSymbol extends Symbol {
  var vals = Map[Name,ValSymbol]()
  def valsList = vals.values.toList
  private val missingVals = scala.collection.mutable.Map[Name, ValSymbol]()
  def lookupValWithMissing(name: Name) = vals.get(name).orElse { missingVals.get(name) }
  def lookupValOrCreateMissing(name: Name) = vals.get(name).getOrElse {
    missingVals.getOrElseUpdate(name, new ValSymbol(this, name));
  }
  val template : TemplateSymbol
  def declaredVals = vals
  def valsAlphabeticOrder = (vals.values ++ missingVals.values).toList.sortBy(_.name.str)
  var executionOrder = List[ValSymbol]()

  object connections extends Namer {
    var junctions = Set[Junction]()
    def usedNames = junctions map { _.name.str }
    var flow = Map[PortInstance, Set[PortInstance]]()
    var clumps = Buffer[Clump]()
    def clumpOf(c: ConnectionDef) = clumps find { _.connections.contains(c) }
    def clumpOf(p: PortSide) = clumps find { _.ports.contains(p) }
    def clumpOf(j: Junction) = clumps find { _.junctions.contains(j) }
    def addPort(j: Junction, a: PortRef, c: ConnectionDef) {
      val pk = PortSide.findOrCreateMissing(a, BlockSymbol.this)
      val newClump = merge(clumpOf(pk), clumpOf(j))
      newClump.connections += c
      newClump.ports += pk
      newClump.junctions += j
    }
    def addPorts(a: PortRef, b: PortRef, c: ConnectionDef) {
      val (as, bs) = (PortSide.findOrCreateMissing(a, BlockSymbol.this), PortSide.findOrCreateMissing(b, BlockSymbol.this))
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
          val clump = Clump(Set(), Set(), Set(), BlockSymbol.this)
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
}
class BoxTypeSymbol(
  val name: Name, //Class name without package
  val pkg: Name, // pkgdecl
  val superName: Option[Name], //fqname
  val image: Option[String],
  var visualClass: Option[Name],
  val abstractCl: Boolean = false) extends TemplateSymbol with BoxType {
  
  
  type B = ReferenceBinding
  var javaScope: ZaluumClassScope = null
  var scope : Scope = null
  val owner = null
  var hasApply = false
  var _superSymbol: Option[BoxType] = None
  var okOverride = false
  var constructors = List[Constructor]()
  var params = Map[Name,ParamSymbol]()
  var source: String = "" // TODO
  tpe = this
  def portsWithSuper: Map[Name, PortSymbol] = ports ++ superSymbol.map { _.portsWithSuper }.getOrElse(Map())
  def fqName: Name = if (pkg.str != "") Name(pkg.str + "." + name.str) else name
  def block = blocks.head
  def superSymbol = {
    _superSymbol match {
      case Some(s) ⇒ _superSymbol
      case None ⇒ superName match {
        case Some(sn) ⇒
          _superSymbol = scope.lookupBoxType(sn)
          _superSymbol
        case None ⇒ None
      }
    }
  }
  def IOInOrder = (ports.values ++ params.values).toList.sortBy(_.name.str)
  def paramsInOrder = params.values.toList.sortBy(_.name.str)
  def lookupPortWithSuper(name: Name): Option[PortSymbol] =
    super.lookupPortWithSuper(name) orElse (superSymbol flatMap { _.lookupPortWithSuper(name) })
}

//class ConnectionSymbol(val owner:Symbol, val name:Name, val from:Tree, val to:Tree) extends Symbol 
// TODO make two classes one that has values from the declaring tree and the other directly from symbol
class IOSymbol(val owner: TemplateSymbol, val name: Name, val dir: PortDir) extends Symbol {
  def box = owner
}
class PortSymbol(owner: TemplateSymbol, name: Name, val extPos: Point, dir: PortDir) extends IOSymbol(owner, name, dir) {
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
class PortInstance(val name: Name, val valSymbol: ValSymbol) {
  var portSymbol: Option[PortSymbol] = None
  var missing = false
  var connectedFrom: Option[PortInstance] = None
  var blameConnection: Option[ConnectionDef] = None
  var finalTpe: Type = NoSymbol
  override def toString = "PortInstance(" + portSymbol + ", " + valSymbol + ")"
}

object PortSide {
  def find(p: PortRef, bl: BlockSymbol) = {
    p.fromRef match {
      case t: ThisRef ⇒ bl.template.thisVal.findPortSide(p)
      case v: ValRef ⇒ for (vs <- bl.lookupValWithMissing(v.name); ps <- vs.findPortSide(p)) yield ps
    }
  }
  def findOrCreateMissing(p: PortRef, bl: BlockSymbol) = {
    def createMissing(vs: ValSymbol, inside: Boolean) = {
      val missing = new PortInstance(p.name, vs)
      missing.missing = true
      val side = new PortSide(missing, p.in, inside)
      vs.portInstances ::= missing
      vs.portSides ::= side
      side
    }
    p.fromRef match {
      case t: ThisRef =>
        bl.template.thisVal.findPortSide(p).getOrElse {
          createMissing(bl.template.thisVal, true)
        }
      case v: ValRef =>
        val vs = bl.lookupValOrCreateMissing(v.name)
        vs.findPortSide(p).getOrElse { createMissing(vs, false) }
    }
  }
}
class PortSide(val pi: PortInstance, val inPort: Boolean, val fromInside: Boolean) {
  def flowIn = if (fromInside) !inPort else inPort
  def name = pi.name
  def toRef = {
    if (fromInside) PortRef(ThisRef(), name, inPort) // ok?
    else PortRef(ValRef(pi.valSymbol.name), name, inPort)
  }
  override def toString() = "PortSide(" + pi.toString + ", in=" + inPort + ", fromInside=" + fromInside + ")"
}
class ValSymbol(val owner: BlockSymbol, val name: Name) extends Symbol {
  var params = Map[ParamSymbol, Any]()
  var info: AnyRef = null
  var constructor: Option[Constructor] = None
  var constructorParams = List[(Any, Type)]()
  var portInstances = List[PortInstance]()
  var portSides = List[PortSide]()
  private def createPs(name: Name, dir: Boolean) = {
    val pi = new PortInstance(name, this)
    val ps = new PortSide(pi, dir, false)
    portInstances ::= pi
    portSides ::= ps
    ps
  }
  def createIn(name: Name) = createPs(name, true)
  def createOut(name: Name) = createPs(name, false)
  def findPortInstance(p: PortSymbol): Option[PortInstance] = {
    portInstances.find(_.portSymbol == Some(p))
  }
  def findPortSide(pr: PortRef) =
    portSides.find(ps => ps.pi.name == pr.name && ps.inPort == pr.in)
  def findPortSide(p: PortSymbol) =
    portSides.find(
      ps => ps.pi match {
        case r: PortInstance => r.portSymbol == Some(p)
        case _ => false
      })

  override def toString = "ValSymbol(" + name + ")"
}

