package org.zaluum.nide.compiler

import javax.swing.JPanel
import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.experimental.dag.DirectedAcyclicGraph.CycleFoundException
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.experimental.dag.DirectedAcyclicGraph
import scala.collection.mutable.Buffer
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation
class CompilationException extends Exception

class Reporter {
  case class Error(msg: String, mark: Option[Int])
  val errors = Buffer[Error]()
  def report(str: String, mark: Option[Int] = None) {
    errors += Error(str, mark)
  }
  def check() {
    if (!errors.isEmpty)
      fail
  }
  def fail = throw new CompilationException()

  def fail(err: String, mark: Option[Int] = None): Nothing = {
    report(err)
    fail
  }
  def apply(assertion: Boolean, res: ⇒ String, mark: Option[Int] = None, fail: Boolean = false) {
    if (!assertion) report(res) // TODO mark
    if (fail) check()
  }
  def apply() = check()
  override def toString = errors.toString
}
case class Name(str: String) {
  def classNameWithoutPackage = str.split('.').last
  def toRelativePath: String = str.replace('.', '/')
  def toRelativePathClass = toRelativePath + ".class"
  def internal = str.replace('.', '/')
  def asArray: Option[(Name, Int)] = {
    val i = str.indexOf('[')
    if (i == -1) None
    else {
      val name = str.substring(0, i).trim
      var dim = 0
      var readOpen = false
      for (j ← i until str.length) {
        str(j) match {
          case ' ' ⇒
          case '[' ⇒
            dim += 1
            if (readOpen) return None
            readOpen = true
          case ']' ⇒
            if (!readOpen) return None
            readOpen = false
          case _ ⇒ return None
        }
      }
      if (!readOpen && dim > 0 && name.length > 0)
        Some((Name(name), dim))
      else None
    }
  }
}
object Literals {
  def parse(value: String, tpe: Name): Option[Any] = {
    try {
      Some(tpe match {
        case Name("byte") ⇒ value.toByte
        case Name("short") ⇒ value.toShort
        case Name("int") ⇒ value.toInt
        case Name("float") ⇒ value.toFloat
        case Name("double") ⇒ value.toDouble
        case Name("boolean") ⇒ value.toBoolean
        case Name("java.lang.String") ⇒ value
        case Name("char") ⇒ value.charAt(0)
      })
    } catch {
      case e ⇒ None
    }
  }
}
trait Scope {
  def alreadyDefinedBoxType(name: Name): Boolean
  def lookupPort(name: Name): Option[PortSymbol]
  def lookupVal(name: Name): Option[ValSymbol]
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[BoxType]
  def lookupBoxTypeLocal(name: Name): Option[BoxType]
  def enter(sym:ValSymbol) 
  def enter(sym:PortSymbol) 
  def enter(sym:ParamSymbol) 
  def enter(sym:BoxTypeSymbol) 
  def root: Symbol
}
trait RootSymbol extends Scope with Symbol {
  val owner = NoSymbol
  val name = null
  scope = this
  private def fail = throw new UnsupportedOperationException()
  def lookupPort(name: Name): Option[PortSymbol] = fail
  def lookupVal(name: Name): Option[ValSymbol] = fail
  def lookupParam(name: Name): Option[ParamSymbol] = fail
  def lookupBoxTypeLocal(name: Name): Option[BoxType] = fail
  def root: Symbol = this
  object primitives {
    private def n(str: String, desc: String) = new PrimitiveJavaType(root, Name(str), desc)
    val byte = n("byte", "B")
    val short = n("short", "S")
    val int = n("int", "I")
    val long = n("long", "J")
    val float = n("float", "F")
    val double = n("double", "D")
    val boolean = n("boolean", "Z")
    val char = n("char", "C")
    val allTypes = List(byte, short, int, long, float, double, boolean, char)
    def find(desc: String) = allTypes.find(_.descriptor == desc)
    def find(name: Name) = allTypes.find(_.name == name)
  }
}
class FakeGlobalScope(realGlobal: Scope) extends LocalScope(realGlobal) { // for presentation compiler
  case object fakeRoot extends Symbol {
    val owner = NoSymbol
    scope = FakeGlobalScope.this
    override val name = null
  }
  override val root = fakeRoot
}
class LocalScope(val enclosingScope: Scope) extends Scope with Namer {
  protected var ports = Map[Name, PortSymbol]()
  protected var params = Map[Name, ParamSymbol]()
  protected var vals = Map[Name, ValSymbol]()
  protected var boxes = Map[Name, BoxType]()
  def lookupPort(name: Name): Option[PortSymbol] = ports.get(name)
  def lookupParam(name: Name): Option[ParamSymbol] = params.get(name)
  def lookupVal(name: Name): Option[ValSymbol] = vals.get(name)
  def lookupType(name: Name): Option[Type] = enclosingScope.lookupType(name)
  def alreadyDefinedBoxType(name: Name): Boolean = boxes.get(name).isDefined
  def lookupBoxType(name: Name): Option[BoxType] =
    boxes.get(name) orElse { enclosingScope.lookupBoxType(name) }
  def lookupBoxTypeLocal(name: Name): Option[BoxType] = boxes.get(name)
  def enter(sym:ValSymbol) = { vals += (sym.name -> sym); sym }
  def enter(sym:PortSymbol) = { ports+=(sym.name-> sym); sym }
  def enter(sym:BoxTypeSymbol) = { boxes+=(sym.name->sym); sym }
  def enter(sym:ParamSymbol) = { params += (sym.name->sym); sym }
  def usedNames = (boxes.keySet.map { _.str } ++ vals.keySet.map { _.str } ++ ports.keySet.map { _.str }).toSet
  def root = enclosingScope.root
}
trait ReporterAdapter {
  def location(tree: Tree): Int
  def reporter: Reporter
  def error(str: String, tree: Tree) = reporter.report(str, Some(location(tree)))
}
class Analyzer(val reporter: Reporter, val toCompile: BoxDef, val global: Scope) {
  def globLocation(t: Tree) = t.line
  
  class Namer(initOwner: Symbol) extends Traverser(initOwner) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def defineBox(symbol: BoxTypeSymbol, tree: Tree) {
  	  prepare(symbol, currentScope, tree, currentScope.alreadyDefinedBoxType(symbol.name)) {
    	currentScope.enter(symbol)
      }
    }
    def defineVal(symbol: ValSymbol, tree: Tree) {
  	  prepare(symbol, currentScope, tree, currentScope.lookupVal(symbol.name).isDefined) {
    	currentScope.enter(symbol)
      }
    }
    def definePort(symbol: PortSymbol, tree: Tree) {
   	  prepare(symbol, currentScope, tree, /* FIXME currentScope.lookupPort(symbol.name).isDefined*/ false) {
    	currentScope.enter(symbol)
      }
    }
    private def prepare(symbol:Symbol, scope:Scope, tree:Tree, dupl:Boolean)(block: =>Unit) {
      if (dupl) error("Duplicate symbol " + symbol.name, tree)
      symbol.scope = scope
      tree.scope = scope
      tree.symbol = symbol
      symbol.decl = tree
    }
    override def traverse(tree: Tree) {
      tree match {
        case b: BoxDef ⇒
          val cl = Some(Name(classOf[JPanel].getName))
          val sym = new BoxTypeSymbol(currentOwner, b.name, b.pkg, b.superName, b.image, cl)
          sym.hasApply = true
          defineBox(sym, tree)
          sym.constructors = List(new Constructor(sym, List()))
          tree.tpe = sym
        // FIXME reported errors do not show in the editor (valdef)
        case p @ PortDef(name, typeName, dir, inPos, extPos) ⇒
          definePort(new PortSymbol(currentOwner.asInstanceOf[BoxType], name, extPos, dir), tree) // owner of a port is boxtypesymbol
        case v: ValDef ⇒
          defineVal(new ValSymbol(currentOwner.asInstanceOf[BoxTypeSymbol], v.name), tree)
        case _ ⇒
          tree.scope = currentScope
      }
      super.traverse(tree)
    }
  }
  object expressionTyper {
    def apply(typeName: Name, owner: Symbol): Option[BoxType] = typeName match {
      case Name("org.zaluum.math.Sum") => Some(new SumExprType(owner))
      case _ => None
    }
  }
  class Resolver(global: Symbol) extends Traverser(global) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def createPortInstances(bs: BoxType, vsym: ValSymbol, isThis: Boolean) = {
      val rpis = (for (p <- bs.portsWithSuper.values; if p.isInstanceOf[PortSymbol]) yield {
        new RealPortInstance(p, vsym)
      }).toList;
      vsym.portInstances = rpis
      vsym.portSides = (for (pi <- rpis) yield {
        pi.portSymbol.dir match {
          case In => List(new PortSide(pi, true, isThis))
          case Out => List(new PortSide(pi, false, isThis))
          case Shift => List(new PortSide(pi, true, isThis), new PortSide(pi, false, isThis))
        }
      }).flatMap(a => a);
    }
    private def catchAbort[T](b: ⇒ Option[T]): Option[T] =
      try { b } catch { case e: AbortCompilation ⇒ None }
    override def traverse(tree: Tree) {
      super.traverse(tree)
      tree match {
        case b: BoxDef ⇒
          val bs = b.sym
          b.superName foreach { sn ⇒
            catchAbort(currentScope.lookupBoxType(sn)) match {
              case Some(sbs: BoxTypeSymbol) ⇒
                bs._superSymbol = Some(sbs)
              /* if (!bs.okOverride) 
                  error ("Super box " + sn.str + " has no 'void contents()' to override or has other abstract methods.", tree)*/
              case None ⇒
                error("Super box type not found " + sn, tree)
            }
          }
          bs.thisVal = new ValSymbol(currentOwner.asInstanceOf[BoxTypeSymbol], Name("this"))
          bs.thisVal.tpe = bs
          createPortInstances(bs, bs.thisVal, true)
        case PortDef(name, typeName, in, inPos, extPos) ⇒
          tree.symbol.tpe = catchAbort(currentScope.lookupType(typeName)) getOrElse {
            error("Port type not found " + typeName, tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case v: ValDef ⇒
          val vsym = v.sym
          catchAbort(expressionTyper(v.typeName, currentOwner) orElse currentScope.lookupBoxType(v.typeName)) match {
            case Some(bs: BoxTypeSymbol) ⇒
              v.symbol.tpe = bs
              if (!bs.hasApply) {
                error("Box " + v.typeName.str + " has no apply method", tree)
              }
              // Constructor
              val consSign = v.constructorTypes map { name ⇒
                currentScope.lookupType(name) getOrElse {
                  error("Constructor type " + name + " not found", tree)
                  NoSymbol
                }
              }
              bs.constructors.find { _.matchesSignature(consSign) } match {
                case Some(cons) ⇒
                  vsym.constructor = Some(cons)
                  vsym.constructorParams = v.constructorParams.zip(consSign) map {
                    case (value, tpe) ⇒
                      val parsed = Literals.parse(value, tpe.name) getOrElse {
                        error("Cannot parse literal \"" + value + "\" to " + tpe.name.str, tree)
                        null
                      }
                      (parsed, tpe)
                  }
                case None ⇒
                  error("Cannot find constructor for box " + v.typeName.str +
                    " with signature (" + v.constructorTypes.map { _.str }.mkString(", ") + ")", tree)
              }
              // params
              for (p ← v.params.asInstanceOf[List[Param]]) {
                bs.lookupParam(p.key) match {
                  case Some(parSym) ⇒
                    val toType = parSym.tpe.name
                    val parsed = Literals.parse(p.value, toType) getOrElse {
                      error("Cannot parse literal \"" + p.value + "\" to " + toType.str + " in parameter " + p.key, tree)
                      null
                    }
                    vsym.params += (parSym -> parsed)
                  case None ⇒ error("Cannot find parameter " + p.key, tree)
                }
              }
              createPortInstances(bs, vsym, false)
              vsym.params
            case Some(b: SumExprType) =>
              v.symbol.tpe = b
              b.a.tpe = currentScope.lookupType(Name("int")).get
              b.b.tpe = currentScope.lookupType(Name("int")).get
              b.c.tpe = currentScope.lookupType(Name("int")).get
              createPortInstances(b, vsym, false)
            case a ⇒
              v.symbol.tpe = NoSymbol
              error("Box class " + v.typeName + " not found", tree);
          }
          // constructor match
          tree.tpe = tree.symbol.tpe
        case ValRef(name) ⇒
          tree.symbol = catchAbort(currentScope.lookupVal(name)) getOrElse {
            error("Box not found " + name, tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case p @ PortRef(fromTree, name, in) ⇒ // TODO filter in?
          tree.symbol = fromTree.tpe match {
            case b: BoxType ⇒
              catchAbort(b.lookupPort(name)).getOrElse {
                error("Port not found " + name + " in box type " + b, tree);
                NoSymbol
              }
            case tpe ⇒ NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case ThisRef() ⇒
          tree.symbol = currentOwner // TODO what symbol for this?
          tree.tpe = currentOwner.asInstanceOf[BoxTypeSymbol] // owner of thisRef is owner of connection which is boxTypeSymbol
        case _ ⇒
      }
    }
  }

  class CheckConnections(b: BoxDef, owner: Symbol) {
    val bs = b.sym
    val acyclic = new DirectedAcyclicGraph[ValSymbol, DefaultEdge](classOf[DefaultEdge])
    var usedInputs = Set[PortSide]()
    def check() = Checker.traverse(b)
    object Checker extends Traverser(owner) with ReporterAdapter {
      def location(tree: Tree) = globLocation(tree)
      def reporter = Analyzer.this.reporter
      override def traverse(tree: Tree) {
        tree match {
          case b: BoxDef ⇒
            traverseTrees(b.vals)
            traverseTrees(b.junctions)
            traverseTrees(b.connections)
            b.defs foreach {
              case bDef: BoxDef ⇒
                new CheckConnections(bDef, b.symbol).check()
            }
            check()
          case v: ValDef ⇒ acyclic.addVertex(v.sym) // valdefs always have symbol
          case j @ Junction(name, _) ⇒
            bs.connections.lookupJunction(name) match {
              case Some(j) ⇒ error("junction name already exists", j)
              case None ⇒ bs.connections.junctions += j
            }
          case c @ ConnectionDef(a, b, waypoints) ⇒
            if (a == EmptyTree || b.symbol == EmptyTree) {
              error("incomplete connection " + a + "<->" + b, tree)
            } else {
              bs.connections.addConnection(c)
              println(c)
            }
          case _ ⇒
        }
      }
      private def check() {
        def checkClump(c: Clump) {
          val ins = c.ports.filter(p ⇒ p.flowIn)
          val outs = c.ports.filter(p ⇒ !p.flowIn)
          if (outs.size == 0) error("No output connected", c.connections.head)
          else if (outs.size > 1) error("More than one output is connected", c.connections.head)
          else if (ins.size == 0) error("No inputs connected", c.connections.head)
          else {
            if (!usedInputs.intersect(ins).isEmpty) error("input connected multiple times", c.connections.head) // TODO check online to identify offending connection 
            usedInputs ++= ins
            // check types
            val types = for (ps ← c.ports) yield {
              ps.pi match {
                case r: RealPortInstance => r.portSymbol.tpe
                case _ => NoSymbol
              }
            }
            if (types.size != 1) error("Connection with incompatible types " + types.mkString(","), c.connections.head)
            else {
              c.connections foreach { _.tpe = types.head }
              c.junctions foreach { _.tpe = types.head }
            }
            // check graph consistency
            val out = outs.head
            bs.connections.flow += (out -> ins)
            def addDag(vout: PortInstance, vin: PortInstance) {
              try {
                acyclic.addDagEdge(vout.valSymbol, vin.valSymbol);
              } catch {
                case e: CycleFoundException ⇒ error("cycle found ", c.connections.head)
                case e: IllegalArgumentException ⇒ error("loop found", c.connections.head)
              }
            }
            import org.zaluum.nide.RichCast._
            for (in ← ins) {
              if (!out.fromInside && !in.fromInside)
                addDag(out.pi, in.pi)
            }
          }
        }
        val resolved = b.connections.forall {
          case con: ConnectionDef ⇒
            def checkResolved(p: Tree) = p match {
              case EmptyTree ⇒ error("Wire is not connected", con); false
              case j: JunctionRef ⇒ /*if(!bs.junctions.exists {_.name == j.name}) {
                  error("FATAL: junction does not exists " + j,con)
                }*/ true
              case p: PortRef ⇒
                PortSide.find(p, bs) match {
                  case None ⇒ error("Cannot find port " + p, con); false
                  case _ ⇒ true
                }
            }
            checkResolved(con.a) && checkResolved(con.b)
        }
        if (resolved) {
          bs.connections.clumps foreach { checkClump(_) }
          import scala.collection.JavaConversions._
          val topo = new TopologicalOrderIterator(acyclic);
          bs.executionOrder = topo.toList
        }
      }
    }
  }
  def runNamer(): Tree = {
    val root = global.root
    new Namer(root).traverse(toCompile)
    toCompile
  }
  def runResolve(): Tree = {
    new Resolver(global.root).traverse(toCompile)
    toCompile
  }
  def runCheck(): Tree = {
    new CheckConnections(toCompile, global.root).check()
    toCompile
  }
}
