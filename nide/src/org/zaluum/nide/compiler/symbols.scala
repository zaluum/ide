package org.zaluum.nide.compiler

import org.zaluum.nide.zge.ParamProperty
import org.zaluum.nide.zge.BeanProperty
import org.zaluum.nide.zge.Property
import org.zaluum.nide.zge.Controller
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.DirectedGraph
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
  private var _tpe: JavaType = NoSymbol
  def tpe = _tpe
  def tpe_=(t: JavaType) { _tpe = t }
  override def toString = "Symbol(" + (if (name != null) name.str else "NoSymbol") + ")"
}
case object NoSymbol extends Symbol with JavaType {
  type B = ReferenceBinding
  val owner = NoSymbol
  def descriptor = null
  val name = Name("NoSymbol")
  def fqName = name
  val binding = null
  def loadClass(cl: ClassLoader) = None
}
trait JavaType extends Symbol {
  def fqName: Name
  def javaSize = 1
  type B <: TypeBinding
  def binding: B
  val owner: Symbol
  def name: Name
  def descriptor: String
  override def toString = "JavaType(" + name + ")"
  def loadClass(cl: ClassLoader): Option[Class[_]]
}
class PrimitiveJavaType(
    val name: Name,
    override val descriptor: String,
    override val javaSize: Int,
    val boxedName: Name,
    val boxMethod: String, val binding: BaseTypeBinding, javaclass: Class[_]) extends JavaType {
  type B = BaseTypeBinding
  val owner = null
  val fqName = name
  def loadClass(cl: ClassLoader) = Some(javaclass)
}
class ArrayType(val owner: Symbol, val of: JavaType, val dim: Int, val binding: ArrayBinding) extends JavaType {
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
  def loadClass(cl: ClassLoader) = None
  override def toString = "ArrayType(" + of.toString + ", " + dim + ")"
}
class ParamSymbol(val owner: JavaType, val name: Name) extends Symbol {
  override def toString = "ParamSymbol(" + name + ")"
  def fqName = name
}
class BeanParamSymbol(
    owner: JavaType,
    val getter: MethodBinding,
    val setter: MethodBinding,
    initTpe: JavaType) extends ParamSymbol(owner, Name(MethodHelper.propertyName(getter))) {
  tpe = initTpe;
  def declaringClass = getter.declaringClass.compoundName.map { _.mkString }.mkString(".")
}
object MethodHelper {
  def isGetter(m: MethodBinding) = {
    val name = m.selector.mkString
    m.isPublic() &&
      (m.parameters == null || m.parameters.size == 0) &&
      (
        (name.size > 3 &&
          name.startsWith("get") &&
          m.returnType != TypeBinding.VOID) ||
          (name.size > 2 &&
            name.startsWith("is") &&
            m.returnType == TypeBinding.BOOLEAN))

  }
  def isSetter(m: MethodBinding) = {
    m.isPublic() &&
      m.selector.mkString.size > 3 &&
      m.selector.mkString.startsWith("set") &&
      m.returnType == TypeBinding.VOID &&
      m.parameters.size == 1
  }
  def propertyName(m: MethodBinding) = {
    val s = m.selector
    if ((s(0) == 'g' || s(0) == 's') && s(1) == 'e' && s(2) == 't') {
      s(3).toLower + s.mkString.drop(4)
    } else {
      s(2).toLower + s.mkString.drop(3)
    }
  }

}
trait ClassJavaType extends JavaType {
  type B = ReferenceBinding
  val binding: B
  val scope: ZaluumClassScope
  def descriptor = "L" + fqName.internal + ";"
  lazy val name = Name(binding.compoundName.last.mkString)
  lazy val pkg = Name(binding.fPackage.compoundName.map(_.mkString).mkString("."))
  lazy val fqName: Name = if (pkg.str != "") Name(pkg.str + "." + name.str) else name
  lazy val engine = ZaluumCompletionEngineScala.engineFor(scope)
  def allMethods = ZaluumCompletionEngineScala.allMethods(engine, scope, binding, static = false)
  def allFields = ZaluumCompletionEngineScala.allFields(engine, scope, binding, static = false)
  def allConstructors = ZaluumCompletionEngineScala.allConstructors(engine, scope, binding)
  lazy val beanProperties = {
    val map = scala.collection.mutable.HashMap[String, (MethodBinding, MethodBinding)]()
    for (m ← allMethods) {
      if (MethodHelper.isGetter(m)) {
        val name = MethodHelper.propertyName(m)
        map.get(name) match {
          case Some((g, s)) ⇒ map(name) = (m, s)
          case _            ⇒ map(name) = (m, null)
        }
      } else if (MethodHelper.isSetter(m)) {
        val name = MethodHelper.propertyName(m)
        map.get(name) match {
          case Some((g, s)) ⇒ map(name) = (g, m)
          case _            ⇒ map(name) = (null, m)
        }
      }
    }
    var l = List[BeanParamSymbol]()
    for ((name, (g, s)) ← map; if (g != null && s != null)) {
      l ::= new BeanParamSymbol(this, g, s, scope.getJavaType(g.returnType))
    }
    l.sortBy(_.name.str)
  }
  def loadClass(cl: ClassLoader) = try { Some(cl.loadClass(fqName.str)) }
  catch { case e: Exception ⇒ println(e); None }
}

class SimpleClassJavaType(val owner: Symbol, val binding: ReferenceBinding, val scope: ZaluumClassScope) extends ClassJavaType
trait PropertySourceType {
  def properties(controller: Controller, valDef: ValDef): List[ParamProperty]
}
class BoxTypeSymbol(
  val image: Option[String],
  var isVisual: Boolean,
  val binding: ReferenceBinding,
  val scope: ZaluumClassScope) extends ClassJavaType
    with TemplateSymbol with BoxType with Namer with PropertySourceType {
  tpe = this
  val owner = null
  var initMethod: Option[MethodBinding] = None
  var hasApply = false
  var constructors = List[Constructor]()
  var methodSelector: Name = _
  override def tdecl: BoxDef = decl.asInstanceOf[BoxDef]
  override def templateTree = tdecl.template
  def onlyVisual = !hasApply && isVisual
  def usedNames = usedValNames ++ (ports.keySet map { _.str })
  def usedValNames = (block :: block.deepBlocks).flatMap { _.usedValNames }.toSet
  def block = blocks.head
  def argsInOrder = ports.values.toList filter { p ⇒ p.dir == In } sortBy { _.name.str }
  def returnPort = ports.values.toList find { p ⇒ p.dir == Out && !p.isField }
  def fieldReturns = ports.values.toList filter { p ⇒ p.isField && p.dir == Out } sortBy { _.name.str }
  def returnDescriptor = returnPort map { _.tpe.fqName.descriptor } getOrElse ("V")
  def methodSignature = "(" + argsInOrder.map { _.tpe.fqName.descriptor }.mkString + ")" + returnDescriptor
  def mainBS = this
  def properties(controller: Controller, valDef: ValDef): List[ParamProperty] = {
    for (
      b ← beanProperties;
      val t = Values.typeFor(b);
      if (!t.isInstanceOf[InvalidValueType])
    ) yield new BeanProperty(controller, valDef, b)
  }
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
  var blocks = List[BlockSymbol]() // same order as tree
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
  var thisVal: ValSymbol = _ // should be template
  def mainBS: BoxTypeSymbol
}
trait BoxType extends TemplateSymbol with JavaType {
  def templateTree: Template
}
class BlockSymbol(val template: TemplateSymbol) extends Symbol with Namer {
  def name = fqName
  def fqName = Name(template.thisVal.fqName.str + "_block" + blockNumeral)
  def owner = template
  var vals = Map[Name, ValSymbol]()
  var executionOrder = List[ValSymbol]()
  val dag = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
  val execPaths = Buffer[ExecutionPath]()
  def secondaryPaths = execPaths.drop(1)
  def mainPath = execPaths(0)
  private val missingVals = scala.collection.mutable.Map[Name, ValSymbol]()

  override def tdecl: Block = decl.asInstanceOf[Block]
  def isMainBSBlock = template.mainBS == template
  def blockNumeral = template.blocks.indexOf(this)
  def uniqueBlock = template.blocks.size == 1
  def usedValNames = (valsList ++ missingVals.values).map(_.name.str).toSet
  def usedNames = (template.mainBS.usedValNames ++ (template.ports.values.map { _.name.str })).toSet

  def rename(valDef: ValDef, newName: Name, labelDesc: Option[LabelDesc], gui: Boolean): EditTransformer = {
    assert(tdecl.valDefs.contains(valDef))
      def renamedEnd(end: Option[ConnectionEnd]) = end match {
        case Some(PortRef(ValRef(valDef.name), b, c)) ⇒ Some(PortRef(ValRef(newName), b, c))
        case other                                    ⇒ other
      }
    new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case v: ValDef if (v == valDef) ⇒
          val label = if (gui) v.label else labelDesc
          val labelGui = if (gui) labelDesc else v.labelGui
          valDef.copy(name = newName, label = label, labelGui = labelGui, params = transformTrees(valDef.params))
        case c @ ConnectionDef(a, b, points) if (tdecl.connections.contains(c)) ⇒
          ConnectionDef(renamedEnd(a), renamedEnd(b), points)
      }
    }
  }
  def deepBlocks: List[BlockSymbol] = {
    var res = List[BlockSymbol]()
    for (vs ← this.valsList; other ← vs.blocks) {
      res ::= other
      res :::= other.deepBlocks
    }
    res
  }
  def valsList = vals.values.toList
  def lookupValWithMissing(name: Name) = vals.get(name).orElse { missingVals.get(name) }
  def lookupValOrCreateMissing(name: Name) = vals.get(name).getOrElse {
    missingVals.getOrElseUpdate(name, new ValSymbol(this, name));
  }
  def valsAlphabeticOrder = (vals.values ++ missingVals.values).toList.sortBy(_.name.str)
  object connections extends Namer {
    var junctions = Set[Junction]()
    var badConnections = Set[ConnectionDef]()
    def usedNames = junctions map { _.name.str }
    var flow = Map[PortInstance, Set[PortInstance]]()
    var connectedFrom = Map[PortInstance, (PortInstance, ConnectionDef)]()
    var clumps = Buffer[Clump]()
    def outgoingConnections(pi: PortInstance): Set[PortInstance] = flow.get(pi).getOrElse(Set())
    def clumpOf(c: ConnectionDef) = clumps find { _.connections.contains(c) }
    def clumpOf(p: PortSide) = clumps find { _.ports.contains(p) }
    def clumpOf(j: Junction) = clumps find { _.junctions.contains(j) }
    def isBad(b: ConnectionDef) = badConnections.contains(b) || b.tpe == NoSymbol
    def markAsBad(b: ConnectionDef) { badConnections += b }
    def addPort(j: Junction, a: PortRef, c: ConnectionDef) {
      val pk = PortSide.findOrCreateMissing(a, BlockSymbol.this)
      a.symbol = pk
      val newClump = merge(clumpOf(pk), clumpOf(j))
      newClump.connections += c
      newClump.ports += pk
      newClump.junctions += j
    }
    def addPorts(a: PortRef, b: PortRef, c: ConnectionDef) {
      val (as, bs) = (PortSide.findOrCreateMissing(a, BlockSymbol.this), PortSide.findOrCreateMissing(b, BlockSymbol.this))
      a.symbol = as
      b.symbol = bs
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

class PortSymbol(val owner: TemplateSymbol, val name: Name, val helperName: Option[Name], val extPos: Point, val dir: PortDir, var isField: Boolean = false) extends Symbol {
  def box = owner
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
  def matchesSignature(sig: List[JavaType]) = sig == params.map { _.tpe }
  def signature = {
    val pars = params map { p ⇒ p.tpe.descriptor } mkString;
    "(" + pars + ")V"
  }
}

class PortInstance(val name: Name, val helperName: Option[Name], val valSymbol: ValSymbol, val dir: PortDir, val portSymbol: Option[PortSymbol] = None) extends Symbol {
  var missing = false
  def isField = portSymbol.map(_.isField).getOrElse(false)
  var internalStorage: StorageType = StorageLocal
  def owner = valSymbol
  def hasDecl = portSymbol.map { _.decl != null } getOrElse { false }
  def declOption = portSymbol.flatMap { p ⇒ Option(p.decl.asInstanceOf[PortDef]) }
  def fqName = Name(valSymbol.fqName.str + "_" + name.str)
  def joinfqName = Name(fqName.str + "_join")
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
      def createMissingPort(vs: ValSymbol, inside: Boolean) = {
        val dir = if (p.in) In else Out // shift ? 
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
          createMissingPort(bl.template.thisVal, inside = true)
        }
      case v: ValRef ⇒
        val vs = bl.lookupValOrCreateMissing(v.name)
        v.symbol = vs
        vs.findPortSide(p, inside = false).getOrElse { createMissingPort(vs, inside = false) }
    }
  }
}
class PortSide(val pi: PortInstance, val inPort: Boolean, val fromInside: Boolean) extends Symbol {
  override def tpe = pi.tpe
  override def tpe_=(t: JavaType) { pi.tpe = t }
  def owner = pi
  def flowIn = if (fromInside) !inPort else inPort
  def name = pi.name
  def helperName = pi.helperName
  def toRef = {
    if (fromInside) PortRef(ThisRef(), name, inPort) // ok?
    else PortRef(ValRef(pi.valSymbol.name), name, inPort)
  }
  override def toString() = "PortSide(" + pi.toString + ", in=" + inPort + ", fromInside=" + fromInside + ")"
}
case class ExecutionPath(num: Int, blockSymbol: BlockSymbol) {
  def name = Name(blockSymbol.fqName.str + "_thread" + num)
  def fqName(bs: BoxTypeSymbol) = Name(bs.fqName.str + "#" + name.str)
  def futureFqName = Name("future_" + name.str)
  var instructions = List[ValSymbol]()
  var forkedBy: Option[ValSymbol] = None
  override def toString = "Thread " + num + " -> " + instructions.map(_.toInstructionsSeq).mkString(", ")
}
class ValSymbol(val owner: BlockSymbol, val name: Name) extends TemplateSymbol {
  override def tdecl = decl.asInstanceOf[ValDef]
  def templateTree = tdecl.template.get
  // var refactor
  var execPath: ExecutionPath = null
  var init = false
  val fork = Buffer[ExecutionPath]()
  val join = Buffer[ValSymbol]()
  var params = Map[ParamSymbol, Value]()
  var isJoinPoint = false
  var info: AnyRef = null
  var classinfo: AnyRef = null
  var portInstances = List[PortInstance]()
  var portSides = List[PortSide]()
  var constructor: Option[Constructor] = None
  var constructorParams = List[Value]()
  def mainBS = owner.template.mainBS
  def fqName = name
  def semfqName = Name(fqName.str + "_sem")
  def isExecutable = tpe match {
    case bs: BoxTypeSymbol if bs.onlyVisual ⇒ false
    case _                                  ⇒ true
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
  def toInstructionsSeq = {
    val result = join.map { t ⇒ "Join(" + t.fqName.str + ")" }
    result += "Exec(" + fqName.str + ")"
    result ++= fork.map { t ⇒ "Fork(" + t.num + ")" }
    result.mkString(", ")
  }
  def bounds = params.find {
    case (k, v) ⇒
      k.name == Name("bounds") &&
        v.valueTpe == RectangleValueType &&
        v.valid
  }.map {
    case (k, v) ⇒
      v.parse.asInstanceOf[java.awt.Rectangle]
  }
}
object Namer {
  def uncapitalize(str: String) = {
    if (str == null) null
    else if (str.length == 0) ""
    else {
      val chars = str.toCharArray
      chars(0) = chars(0).toLower
      new String(chars)
    }
  }
  def toIdentifierBase(baseStr: String) = {
    val id = uncapitalize(baseStr.dropWhile(!Character.isJavaIdentifierStart(_)).filter { Character.isJavaIdentifierPart })
    if (id != null && id != "") Some(id) else None
  }
}
trait Namer {
  def usedNames: Set[String]
  def isNameTaken(str: String): Boolean = usedNames.contains(str)
  private def nextName(str: String): String = {
    val digits = str.reverse.takeWhile(_.isDigit).reverse
    val nextValue = if (digits.isEmpty) 1 else digits.toInt + 1
    str.slice(0, str.length - digits.length) + nextValue
  }

  def freshName(baseStr: String) = freshName_(Namer.toIdentifierBase(baseStr).getOrElse("a"))
  @scala.annotation.tailrec
  private final def freshName_(str: String): String = {
    if (!isNameTaken(str)) str
    else freshName_(nextName(str))
  }
}
