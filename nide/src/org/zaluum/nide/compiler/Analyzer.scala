package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation
import javax.swing.JPanel
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.zaluum.nide.eclipse.integration.model.{ZaluumCompilationUnitScope,ZaluumTypeDeclaration}
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitDeclaration

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
  def parseNarrowestLiteral(v: String, zaluumScope:ZaluumCompilationUnitScope) = {
    def parseIntOpt = try { Some(v.toInt) } catch { case e => None }
    def parseDoubleOpt = try { Some(v.toDouble) } catch { case e => None }
    if (v.toLowerCase=="true") Some(true, primitives.Boolean)
    else if (v.toLowerCase=="false") Some(false, primitives.Boolean)
    else if (v.endsWith("f") || v.endsWith("F")) {
      try { Some((v.toFloat, primitives.Float)) } catch { case e => None }
    } else if (v.endsWith("l") || v.endsWith("L")) {
      try { Some((v.dropRight(1).toLong, primitives.Long)) } catch { case e => e.printStackTrace; println ("long fail " + v);None }
    } else if (v.startsWith("\"") && v.endsWith("\"")) {
      
      Some(v.substring(1, v.length - 1), zaluumScope.getZJavaLangString)
    } else {
      parseIntOpt match { // char?
        case Some(i) => Some(narrowestInt(i))
        case None =>
          parseDoubleOpt match {
            case Some(d) => Some(d, primitives.Double)
            case None => Some(v,  zaluumScope.getZJavaLangString)
          }
      }
    }
  }
  def narrowestInt(i: Int): (Int, PrimitiveJavaType) = {
    val p = if (i <= Byte.MaxValue && i >= Byte.MinValue) primitives.Byte
    else if (i <= Short.MaxValue && i >= Short.MinValue) primitives.Short
    else primitives.Int
    (i, p)
  }
}
trait Scope  {
  def alreadyDefinedBoxType(name: Name): Boolean
  def lookupPort(name: Name): Option[PortSymbol]
  def lookupVal(name: Name): Option[ValSymbol]
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[BoxType]
  def lookupBoxTypeLocal(name: Name): Option[BoxType]
  def enter(sym: ValSymbol)
  def enter(sym: PortSymbol)
  def enter(sym: ParamSymbol)
  def enter(sym: BoxTypeSymbol)
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
}
object primitives {
  private def n(str: String, desc: String, b:BaseTypeBinding, boxedName:Name, boxMethod:String, size: Int = 1) = {
    val p = new PrimitiveJavaType(Name(str), desc, size, boxedName, boxMethod)
    p.binding = b
    p
  }
  val Byte = n("byte", "B", TypeBinding.BYTE, Name("java.lang.Byte"), "byteValue")
  val Short = n("short", "S", TypeBinding.SHORT,Name("java.lang.Short"), "shortValue")
  val Int = n("int", "I", TypeBinding.INT,Name("java.lang.Integer"), "intValue")
  val Long = n("long", "J", TypeBinding.LONG, Name("java.lang.Long"), "longValue",2)
  val Float = n("float", "F", TypeBinding.FLOAT, Name("java.lang.Float"), "floatValue")
  val Double = n("double", "D", TypeBinding.DOUBLE, Name("java.lang.Double"), "doubleValue",2)
  val Boolean = n("boolean", "Z", TypeBinding.BOOLEAN,Name("java.lang.Boolean"), "booleanValue")
  val Char = n("char", "C", TypeBinding.CHAR,Name("java.lang.Char"), "charValue")
  
  val allTypes = List(Byte, Short, Int, Long, Float, Double, Boolean, Char)
  def numericTypes = List(Byte, Short, Int, Long, Float, Double, Char)
  def widening(from: PrimitiveJavaType, to: PrimitiveJavaType) = {
    from match {
      case Byte => List(Short, Int, Long, Float, Double).contains(to)
      case Short => List(Int, Long, Float, Double).contains(to)
      case Char => List(Int, Long, Float, Double).contains(to)
      case Int => List(Long, Float, Double).contains(to)
      case Long => List(Float, Double).contains(to)
      case Float => to == Double
      case _ => false
    }
  }
  def toOperationType(t: PrimitiveJavaType): PrimitiveJavaType = {
    t match {
      case Byte => Int
      case Short => Int
      case Char => Int
      case Int => Int
      case Long => Long
      case Float => Float
      case Double => Double
    }
  }
  def getUnboxedType(p: ClassJavaType): Option[PrimitiveJavaType] = {
    p.name.str match {
      case "java.lang.Boolean" => Some(primitives.Boolean)
      case "java.lang.Char" => Some(primitives.Char)
      case "java.lang.Byte" => Some(primitives.Byte)
      case "java.lang.Short" => Some(primitives.Short)
      case "java.lang.Integer" => Some(primitives.Int)
      case "java.lang.Float" => Some(primitives.Float)
      case "java.lang.Double" => Some(primitives.Double)
      case "java.lang.Long" => Some(primitives.Long)
      case _ => None
    }
  }
  /** must be int long float or double (operationtype) */
  def largerOperation(a: PrimitiveJavaType, b: PrimitiveJavaType): PrimitiveJavaType = {
    val l = List(Int, Long, Float, Double)
    l(math.max(l.indexOf(a), l.indexOf(b)))
  }
  def isNumeric(tpe: Type): Boolean = {
    tpe match {
      case p: PrimitiveJavaType if (p != primitives.Boolean) => true
      case j: JavaType => false 
      case _ => false
    }
  }
  def isIntNumeric(tpe:Type) : Boolean =  tpe==primitives.Int || 
        tpe==primitives.Short || 
        tpe==primitives.Byte || 
        tpe==primitives.Char
  def find(desc: String) = allTypes.find(_.descriptor == desc)
  def find(name: Name) = allTypes.find(_.name == name)
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
  def enter(sym: ValSymbol) = { vals += (sym.name -> sym); sym }
  def enter(sym: PortSymbol) = { ports += (sym.name -> sym); sym }
  def enter(sym: BoxTypeSymbol) = { boxes += (sym.name -> sym); sym }
  def enter(sym: ParamSymbol) = { params += (sym.name -> sym); sym }
  def usedNames = (boxes.keySet.map { _.str } ++ vals.keySet.map { _.str } ++ ports.keySet.map { _.str }).toSet
  def root = enclosingScope.root
}
trait ReporterAdapter {
  def location(tree: Tree): Int
  def reporter: Reporter
  def error(str: String, tree: Tree) = reporter.report(str, Some(location(tree)))
}
class Analyzer(val reporter: Reporter, val toCompile: BoxDef, val global: Scope) extends AnalyzerConnections {
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
    private def prepare(symbol: Symbol, scope: Scope, tree: Tree, dupl: Boolean)(block: => Unit) {
      if (dupl) error("Duplicate symbol " + symbol.name, tree)
      symbol.scope = scope
      tree.scope = scope
      tree.symbol = symbol
      symbol.decl = tree
      block
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
   var cud : ZaluumCompilationUnitDeclaration = _
  class Resolver(global: Symbol) extends Traverser(global) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def createPortInstances(bs: BoxType, vsym: ValSymbol, isThis: Boolean) = {
      vsym.portInstances = (for (p <- bs.portsWithSuper.values; if p.isInstanceOf[PortSymbol]) yield {
        val pi = new PortInstance(p.name, vsym)
        pi.portSymbol = Some(p)
        pi
      }).toList;
      vsym.portSides = (for (pi <- vsym.portInstances; ps <- pi.portSymbol) yield {
        ps.dir match {
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
          bs.thisVal = new ValSymbol(bs, Name("this"))
          bs.thisVal.tpe = bs
          createPortInstances(bs, bs.thisVal, true)
        case PortDef(name, typeName, in, inPos, extPos) ⇒
          tree.symbol.tpe = catchAbort(currentScope.lookupType(typeName)) getOrElse {
            error("Port type not found " + typeName, tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case v: ValDef ⇒
          val vsym = v.sym
          catchAbort(Expressions.find(v.typeName) orElse currentScope.lookupBoxType(v.typeName)) match {
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
            case Some(b: ExprType) =>
              v.symbol.tpe = b
              for (p ← v.params.asInstanceOf[List[Param]]) {
                b.lookupParam(p.key) match {
                  case Some(parSym) =>
                    vsym.params += (parSym -> p.value) // FIXME always string?
                  case None => error("Cannot find parameter " + p.key,tree)
                }
              }
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
            case b: BoxTypeSymbol ⇒
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

  def runNamer(): Tree = {
    val root = global.root
    new Namer(root).traverse(toCompile)
    toCompile
  }
 
  def runResolve(cud : ZaluumCompilationUnitDeclaration): Tree = {
    this.cud = cud
    new Resolver(global.root).traverse(toCompile)
    toCompile
  }
  def runCheck(): Tree = {
    new CheckConnections(toCompile, global.root).check()
    toCompile
  }
}
class CompilationException extends Exception
