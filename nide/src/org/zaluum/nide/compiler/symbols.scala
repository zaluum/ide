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
  def name: Name
  override def toString = "Symbol(" + (if (name != null) name.str else "null name") + ")"
}
trait DeclSymbol[T <: Tree] extends Symbol {
  var decl: T = _
}
trait TypedSymbol[T <: Type] extends Symbol {
  private var _tpe: Option[T] = None
  def tpe = _tpe
  def tpe_=(t: Option[T]) { _tpe = t }
  def tpe_=(t: T) { _tpe = Some(t) }
  def tpeHumanStr = tpe.map(_.name.str).getOrElse("<No Type>")
}

class ParamDecl(val name: Name) extends TypedSymbol[JavaType] {
  override def toString = "ParamSymbol(" + name + ")"
  def fqName = name
}
class BeanParamDecl(
  val getter: MethodBinding,
  val setter: MethodBinding,
  initTpe: Option[JavaType])
    extends ParamDecl(Name(MethodHelper.propertyName(getter))) {
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

trait PropertySourceType {
  def properties(controller: Controller, valDef: ValDef): List[ParamProperty]
}

case class Clump(
    var junctions: Set[Junction],
    var ports: Set[PortSide],
    var connections: Set[ConnectionDef],
    bl: BlockSymbol) {
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

trait PortsSymbol extends Symbol {
  var ports = Map[Name, PortSymbol]()
  def lookupPort(name: Name) = ports.get(name)
}

trait TemplateSymbol extends PortsSymbol {
  var blocks = List[BlockSymbol]() // same order as tree
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
  def mainBS: BoxSymbol
}

class BlockSymbol(
    val template: TemplateSymbol) extends DeclSymbol[Block] with Namer {

  def name = fqName
  def fqName = Name(template.thisVal.fqName.str + "_block" + blockNumeral)
  var vals = Map[Name, ValSymbol]()
  var executionOrder = List[ValSymbol]()
  val dag = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
  val execPaths = Buffer[ExecutionPath]()
  def secondaryPaths = execPaths.drop(1)
  def mainPath = execPaths(0)
  private val missingVals = scala.collection.mutable.Map[Name, ValSymbol]()

  def isMainBSBlock = template.mainBS == template
  def blockNumeral = template.blocks.indexOf(this)
  def uniqueBlock = template.blocks.size == 1
  def usedValNames = (valsList ++ missingVals.values).map(_.name.str).toSet
  def usedNames = (template.mainBS.usedValNames ++ (template.ports.values.map { _.name.str })).toSet
  def rename(valDef: ValDef, newName: Name): EditTransformer = {
    rename(valDef, newName, valDef.label, false)
  }
  def rename(valDef: ValDef, newName: Name, labelDesc: Option[LabelDesc], gui: Boolean): EditTransformer = {
    assert(decl.valDefs.contains(valDef))
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
        case c @ ConnectionDef(a, b, points) if (decl.connections.contains(c)) ⇒
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
    def isBad(b: ConnectionDef) = badConnections.contains(b) || b.tpe == None
    def markAsBad(b: ConnectionDef) { badConnections += b }
    def addSinglePort(ref: PortRef) = {
      val pk = PortSide.findOrCreateMissing(ref, BlockSymbol.this)
      ref.symbol = pk
      pk
    }
    def addPort(j: Junction, a: PortRef, c: ConnectionDef) {
      val pk = addSinglePort(a)
      val newClump = merge(clumpOf(pk), clumpOf(j))
      newClump.connections += c
      newClump.ports += pk
      newClump.junctions += j
    }
    def addPorts(a: PortRef, b: PortRef, c: ConnectionDef) {
      val (as, bs) = (addSinglePort(a), addSinglePort(b))
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
      c.symbol = BlockSymbol.this
      (c.a, c.b) match {
        case (Some(p: PortRef), Some(j: JunctionRef)) ⇒ addPort(lookupJunction(j.name).getOrElse { throw new RuntimeException("cannot find junction" + j.name) }, p, c)
        case (Some(j: JunctionRef), Some(p: PortRef)) ⇒ addPort(lookupJunction(j.name).get, p, c)
        case (Some(p1: PortRef), Some(p2: PortRef)) ⇒ addPorts(p1, p2, c)
        case (Some(j1: JunctionRef), Some(j2: JunctionRef)) ⇒ addJunctions(lookupJunction(j1.name).get, lookupJunction(j2.name).get, c)
        case (Some(p: PortRef), None) ⇒ addSinglePort(p)
        case (None, Some(p: PortRef)) ⇒ addSinglePort(p)
        case _ ⇒ println("DEBUG: ignored connection in addConnetion " + c)
      }
    }
  }
}

class PortSymbol(
  val portsSymbol: PortsSymbol,
  val name: Name,
  val helperName: Option[Name],
  val extPos: Point,
  val dir: PortDir,
  var isField: Boolean = false)
    extends TypedSymbol[JavaType] with DeclSymbol[PortDef] {
  def this(portsSymbol: PortsSymbol, name: Name, dir: PortDir) =
    this(portsSymbol, name, None, Point(0, 0), dir)
  override def toString = "PortSymbol(" + name + ")"
}

class ConstructorDecl(val params: List[ParamDecl]) {
  override def toString = {
    if (params.isEmpty) "<default>()" else
      params.map(p ⇒ p.name.str + " : " + p.tpeHumanStr).mkString(", ")
  }
  def matchesSignature(sig: List[JavaType]) = sig == params.map { _.tpe }
  def signature = {
    val pars = params map { _.tpe.map(_.descriptor).getOrElse("<missing>") } mkString;
    "(" + pars + ")V"
  }
}

class PortInstance(val name: Name, val helperName: Option[Name],
                   val valSymbol: ValSymbol, val dir: PortDir,
                   var portSymbol: Option[PortSymbol] = None) extends TypedSymbol[JavaType] {
  var missing = false
  def isField = portSymbol.map(_.isField).getOrElse(false)
  var internalStorage: StorageType = StorageLocal
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
class PortSide(
    val pi: PortInstance,
    val inPort: Boolean,
    val fromInside: Boolean) extends Symbol {
  def tpe = pi.tpe
  def tpe_=(t: JavaType) { pi.tpe = t }
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
  def fqName(bs: BoxSymbol) = Name(bs.fqName.str + "#" + name.str)
  def futureFqName = Name("future_" + name.str)
  var instructions = List[ValSymbol]()
  var forkedBy: Option[ValSymbol] = None
  override def toString = "Thread " + num + " -> " + instructions.map(_.toInstructionsSeq).mkString(", ")
}
class ValSymbol(val owner: BlockSymbol, val name: Name)
    extends TemplateSymbol with TypedSymbol[ExprType] with DeclSymbol[ValDef] {
  def templateTree = decl.template.get
  // var refactor
  var execPath: ExecutionPath = null
  var init = false
  var isExecutable = true
  var isVisual = false
  val fork = Buffer[ExecutionPath]()
  val join = Buffer[ValSymbol]()
  private var values = Map[ParamDecl, Value]()
  var isJoinPoint = false
  var typeSpecificInfo: Option[_] = None
  //  FIXME to option
  var classinfo: JavaType = null
  var configurer: Option[ClassJavaType] = None
  var portInstances = List[PortInstance]()
  var portSides = List[PortSide]()
  var constructorParams = List[Value]()
  def allValues = values
  def setValue(p: ParamDecl, v: Value) { values += (p -> v) }
  def getValue(p: ParamDecl) = values.get(p)
  def getStr(p: ParamDecl): Option[String] = getStr(p.fqName)
  def getStr(n: Name): Option[String] = decl.params.find(_.key == n).map(_.valueStr)
  def getList(p: ParamDecl): Option[List[String]] = getList(p.fqName)
  def getList(n: Name): Option[List[String]] = decl.params.find(_.key == n).map(_.values)
  var constructor: Option[MethodBinding] = None
  def mainBS = owner.template.mainBS
  def fqName = name
  def semfqName = Name(fqName.str + "_sem")
  def javaType = classinfo
  private def createOutsidePs(name: Name, dir: Boolean,
                              helperName: Option[Name] = None,
                              port: Option[PortSymbol] = None) = {
    val pdir = if (dir) In else Out
    val pi = new PortInstance(name, helperName, this, pdir, port)
    val ps = new PortSide(pi, dir, false)
    portInstances ::= pi
    portSides ::= ps
    ps
  }
  def createOutsideIn(name: Name, helperName: Option[Name] = None, port: Option[PortSymbol] = None) = createOutsidePs(name, true, helperName, port)
  def createOutsideOut(name: Name, helperName: Option[Name] = None, port: Option[PortSymbol] = None) = createOutsidePs(name, false, helperName, port)
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
  def bounds = getStr(Name("bounds")).flatMap { str ⇒
    val v = RectangleValueType.create(str)
    if (v.valid)
      Some(v.parse.asInstanceOf[java.awt.Rectangle])
    else None
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
