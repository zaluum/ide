package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer

import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
trait Symbol {
  def owner: Symbol
  def name: Name
  var decl: Tree = null
  def tdecl: Tree = decl
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
      case _            ⇒ false
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
    connections.find { con ⇒
        def isEnd(tree: Option[ConnectionEnd]) = {
          tree match {
            case Some(p: PortRef) ⇒
              PortSide.find(p, bl).exists(_.pi == pi)
            case _ ⇒ false
          }
        }
      isEnd(con.a) || isEnd(con.b)
    }
  }
}
sealed trait TemplateSymbol extends Symbol {
  var ports = Map[Name, PortSymbol]()
  var blocks = List[BlockSymbol]()
  def lookupPort(name: Name) = ports.get(name)
  def templateTree: Template
  def currentBlockIndex = {
    val parsed = templateTree.currentBlock.map { c ⇒
      try { c.toInt } catch { case _ ⇒ 0 }
    }.getOrElse(0)
    if (parsed < 0 || parsed >= blocks.length) 0 else parsed
  }
  def currentBlock = blocks(currentBlockIndex)
  def nextBlockIndex = if (currentBlockIndex >= blocks.length - 1) 0 else currentBlockIndex + 1
  def lookupParam(name: Name): Option[ParamSymbol]
  var thisVal: ValSymbol = _ // should be template
}
trait BoxType extends TemplateSymbol with Type {
  def templateTree: Template
}
class BlockSymbol(val template: TemplateSymbol) extends Symbol with Namer {
  def name = null
  def owner = template
  var vals = Map[Name, ValSymbol]()
  var executionOrder = List[ValSymbol]()
  private val missingVals = scala.collection.mutable.Map[Name, ValSymbol]()

  override def tdecl: Block = decl.asInstanceOf[Block]
  def numeral = template.blocks.indexOf(this)
  def usedNames = (valsList ++ missingVals.values ++ template.ports.values).map { _.name.str }.toSet
  def valsList = vals.values.toList
  def lookupValWithMissing(name: Name) = vals.get(name).orElse { missingVals.get(name) }
  def lookupValOrCreateMissing(name: Name) = vals.get(name).getOrElse {
    missingVals.getOrElseUpdate(name, new ValSymbol(this, name));
  }
  def valsAlphabeticOrder = (vals.values ++ missingVals.values).toList.sortBy(_.name.str)
  object connections extends Namer {
    var junctions = Set[Junction]()
    def usedNames = junctions map { _.name.str }
    var flow = Map[PortInstance, Set[PortInstance]]()
    var connectedFrom = Map[PortInstance, (PortInstance, ConnectionDef)]()
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
        case (Some(p: PortRef), Some(j: JunctionRef)) ⇒ addPort(lookupJunction(j.name).getOrElse { throw new RuntimeException("cannot find junction" + j.name) }, p, c)
        case (Some(j: JunctionRef), Some(p: PortRef)) ⇒ addPort(lookupJunction(j.name).get, p, c)
        case (Some(p1: PortRef), Some(p2: PortRef)) ⇒ addPorts(p1, p2, c)
        case (Some(j1: JunctionRef), Some(j2: JunctionRef)) ⇒ addJunctions(lookupJunction(j1.name).get, lookupJunction(j2.name).get, c)
        // FIXME not connected EmptyTrees
        case _ ⇒ println("DEBUG: ignored connection in addConnetion " + c)
      }
    }
  }
}
class BoxTypeSymbol(
    val name: Name, //Class name without package
    val pkg: Name, // pkgdecl
    val image: Option[String],
    var visualClass: Option[Name],
    val abstractCl: Boolean = false) extends TemplateSymbol with BoxType with Namer {

  type B = ReferenceBinding
  var javaScope: ZaluumClassScope = null
  var scope: Scope = null
  val owner = null
  var hasApply = false
  var _superSymbol: Option[BoxType] = None
  var okOverride = false
  var constructors = List[Constructor]()
  var params = Map[Name, ParamSymbol]()
  var source: String = "" // TODO
  tpe = this
  override def tdecl: BoxDef = decl.asInstanceOf[BoxDef]
  override def templateTree = tdecl.template
  def usedNames = ports.keySet map { _.str } // FIXME what else?
  def fqName: Name = if (pkg.str != "") Name(pkg.str + "." + name.str) else name
  def block = blocks.head
  def IOInOrder = (ports.values ++ params.values).toList.sortBy(_.name.str)
  def paramsInOrder = params.values.toList.sortBy(_.name.str)
  def lookupParam(name: Name) = params.get(name)
  def argsInOrder = ports.values.toList filter { p ⇒ p.dir == In } sortBy { _.name.str }
  def returnPort = ports.values.toList find { p ⇒ p.dir == Out && !p.isField }
  def fieldReturns = ports.values.toList filter { p ⇒ p.isField && p.dir == Out } sortBy { _.name.str }
  def methodSelector = returnPort map { _.name } getOrElse (Name(TreeToClass.defaultMethodName))
  def returnDescriptor = returnPort map { _.tpe.fqName.descriptor } getOrElse ("V")
  def methodSignature = "(" + argsInOrder.map { _.tpe.fqName.descriptor }.mkString + ")" + returnDescriptor
}

// TODO make two classes one that has values from the declaring tree and the other directly from symbol
class IOSymbol(val owner: TemplateSymbol, val name: Name, val dir: PortDir) extends Symbol {
  def box = owner
}
class PortSymbol(owner: TemplateSymbol, name: Name, val helperName: Option[Name], val extPos: Point, dir: PortDir, var isField: Boolean = false) extends IOSymbol(owner, name, dir) {
  def this(owner: TemplateSymbol, name: Name, dir: PortDir) =
    this(owner, name, None, Point(0, 0), dir)
  override def toString = "PortSymbol(" + name + ")"
}
//class ResultPortSymbol(owner: TemplateSymbol, name: Name, val helpName: Name, val extPos: Point) extends PortSymbol(owner,name,helpName,extPos,Out)
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
class PortInstance(val name: Name, val helperName: Option[Name], val valSymbol: ValSymbol, val dir: PortDir, val portSymbol: Option[PortSymbol] = None) {
  var missing = false
  var finalTpe: Type = NoSymbol
  def hasDecl = portSymbol.map { _.decl != null } getOrElse { false }
  def fqName = Name(valSymbol.fqName.str + "_" + name.str)
  override def toString = "PortInstance(" + portSymbol + ", " + valSymbol + ")"
}

object PortSide {
  def find(p: PortRef, bl: BlockSymbol) = {
    p.fromRef match {
      case t: ThisRef ⇒ bl.template.thisVal.findPortSide(p, inside = true)
      case v: ValRef  ⇒ for (vs ← bl.lookupValWithMissing(v.name); ps ← vs.findPortSide(p, inside = false)) yield ps
    }
  }
  def findOrCreateMissing(p: PortRef, bl: BlockSymbol) = {
      def createMissing(vs: ValSymbol, inside: Boolean) = {
        val dir = if (p.in) In else Out // FIXME shift ? 
        val missing = new PortInstance(p.name, None, vs, dir)
        missing.missing = true
        val side = new PortSide(missing, p.in, inside)
        vs.portInstances ::= missing
        vs.portSides ::= side
        side
      }
    p.fromRef match {
      case t: ThisRef ⇒
        bl.template.thisVal.findPortSide(p, inside = true).getOrElse {
          createMissing(bl.template.thisVal, inside = true)
        }
      case v: ValRef ⇒
        val vs = bl.lookupValOrCreateMissing(v.name)
        vs.findPortSide(p, inside = false).getOrElse { createMissing(vs, inside = false) }
    }
  }
}
class PortSide(val pi: PortInstance, val inPort: Boolean, val fromInside: Boolean) {
  def flowIn = if (fromInside) !inPort else inPort
  def name = pi.name
  def helperName = pi.helperName
  def toRef = {
    if (fromInside) PortRef(ThisRef(), name, inPort) // ok?
    else PortRef(ValRef(pi.valSymbol.name), name, inPort)
  }
  override def toString() = "PortSide(" + pi.toString + ", in=" + inPort + ", fromInside=" + fromInside + ")"
}
class ValSymbol(val owner: BlockSymbol, val name: Name) extends TemplateSymbol {
  override def tdecl = decl.asInstanceOf[ValDef]
  def templateTree = tdecl.template.get
  def lookupParam(name: Name): Option[ParamSymbol] = sys.error("")
  // var refactor
  var info: AnyRef = null
  var classinfo: AnyRef = null
  var params = Map[ParamSymbol, Any]()
  var portInstances = List[PortInstance]()
  var portSides = List[PortSide]()
  var constructor: Option[Constructor] = None
  var constructorParams = List[(Any, Type)]()
  def fqName: Name = {
    val prefix = owner.template match {
      case b: BoxTypeSymbol ⇒ ""
      case vs: ValSymbol    ⇒ vs.fqName.str
    }
    Name(prefix + owner.numeral + name.str)
  }
  private def createOutsidePs(name: Name, dir: Boolean, helperName: Option[Name] = None) = {
    val pdir = if (dir) In else Out
    val pi = new PortInstance(name, helperName, this, pdir)
    val ps = new PortSide(pi, dir, false)
    portInstances ::= pi
    portSides ::= ps
    ps
  }
  def createOutsideIn(name: Name, helperName: Option[Name] = None) = createOutsidePs(name, true, helperName)
  def createOutsideOut(name: Name, helperName: Option[Name] = None) = createOutsidePs(name, false, helperName)
  def findPortInstance(p: PortSymbol): Option[PortInstance] = {
    portInstances.find(_.portSymbol == Some(p))
  }
  def findPortSide(pr: PortRef, inside: Boolean) =
    portSides.find(ps ⇒ ps.pi.name == pr.name && ps.inPort == pr.in && ps.fromInside == inside)

  override def toString = "ValSymbol(" + name + ")"
}
trait Namer {
  def usedNames: Set[String]
  def isNameTaken(str: String): Boolean = usedNames.contains(str)
  private def nextName(str: String): String = {
    val digits = str.reverse.takeWhile(_.isDigit).reverse
    val nextValue = if (digits.isEmpty) 1 else digits.toInt + 1
    str.slice(0, str.length - digits.length) + nextValue
  }
  def toIdentifierBase(baseStr:String) = {
    val id = baseStr.dropWhile( !Character.isJavaIdentifierStart(_) ).filter{ Character.isJavaIdentifierPart }
    if (id=="") "a" else id
  }
  def freshName(baseStr:String) = freshName_(toIdentifierBase(baseStr))
  @scala.annotation.tailrec
  private final def freshName_(str: String): String = {
    if (!isNameTaken(str)) str
    else freshName_(nextName(str))
  }
}
