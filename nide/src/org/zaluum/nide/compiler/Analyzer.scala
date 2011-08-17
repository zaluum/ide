package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer

import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitDeclaration
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitScope

import javax.swing.JPanel

class Reporter {
  case class Error(msg: String, mark: Option[Int])
  private var reportMoreErrors = true
  def ifErrorsDoNotReportMore() {
    if (errors.size >0) reportMoreErrors=false
  }
  val errors = Buffer[Error]()
  def report(str: String, mark: Option[Int] = None) {
    if (reportMoreErrors) errors += Error(str, mark)
  }
  override def toString = errors.toString
}
case class Name(str: String) {
  def classNameWithoutPackage = str.split('.').last
  def packageProxy = str.split('.').dropRight(1).mkString(".")
  def toRelativePath: String = str.replace('.', '/')
  def toRelativePathClass = toRelativePath + ".class"
  def internal = str.replace('.', '/')
  def descriptor: String =
    asArray match {
      case Some((arrTpe, dim)) ⇒ ("[" * dim) + arrTpe.descriptor
      case None ⇒
        this match {
          case Name("byte")    ⇒ "B"
          case Name("short")   ⇒ "S"
          case Name("int")     ⇒ "I"
          case Name("long")    ⇒ "J"
          case Name("float")   ⇒ "F"
          case Name("double")  ⇒ "D"
          case Name("boolean") ⇒ "Z"
          case Name("char")    ⇒ "C"
          case _               ⇒ "L" + internal + ";"
        }
    }
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
        case Name("byte")             ⇒ value.toByte
        case Name("short")            ⇒ value.toShort
        case Name("int")              ⇒ value.toInt
        case Name("float")            ⇒ value.toFloat
        case Name("double")           ⇒ value.toDouble
        case Name("boolean")          ⇒ value.toBoolean
        case Name("java.lang.String") ⇒ value
        case Name("char")             ⇒ value.charAt(0)
      })
    } catch {
      case e ⇒ None
    }
  }
  def parseNarrowestLiteral(v: String, zaluumScope: ZaluumCompilationUnitScope) = {
      def parseIntOpt = try { Some(v.toInt) } catch { case e ⇒ None }
      def parseDoubleOpt = try { Some(v.toDouble) } catch { case e ⇒ None }
    if (v.toLowerCase == "true") Some(true, primitives.Boolean)
    else if (v.toLowerCase == "false") Some(false, primitives.Boolean)
    else if (v.endsWith("f") || v.endsWith("F")) {
      try { Some((v.toFloat, primitives.Float)) } catch { case e ⇒ None }
    } else if (v.endsWith("l") || v.endsWith("L")) {
      try { Some((v.dropRight(1).toLong, primitives.Long)) } catch { case e ⇒ e.printStackTrace; println("long fail " + v); None }
    } else if (v.startsWith("\"") && v.endsWith("\"")) {

      Some(v.substring(1, v.length - 1), zaluumScope.getZJavaLangString)
    } else {
      parseIntOpt match { // char?
        case Some(i) ⇒ Some(narrowestInt(i))
        case None ⇒
          parseDoubleOpt match {
            case Some(d) ⇒ Some(d, primitives.Double)
            case None    ⇒ Some(v, zaluumScope.getZJavaLangString)
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
trait Scope extends Symbol {
  def lookupType(name: Name): Option[Type]
  def lookupBoxType(name: Name): Option[BoxType]
  def javaScope: ZaluumCompilationUnitScope
}
object primitives {
  private def n(str: String, desc: String, b: BaseTypeBinding, boxedName: Name, boxMethod: String, size: Int = 1) = {
    val p = new PrimitiveJavaType(Name(str), desc, size, boxedName, boxMethod)
    p.binding = b
    p
  }
  val Byte = n("byte", "B", TypeBinding.BYTE, Name("java.lang.Byte"), "byteValue")
  val Short = n("short", "S", TypeBinding.SHORT, Name("java.lang.Short"), "shortValue")
  val Int = n("int", "I", TypeBinding.INT, Name("java.lang.Integer"), "intValue")
  val Long = n("long", "J", TypeBinding.LONG, Name("java.lang.Long"), "longValue", 2)
  val Float = n("float", "F", TypeBinding.FLOAT, Name("java.lang.Float"), "floatValue")
  val Double = n("double", "D", TypeBinding.DOUBLE, Name("java.lang.Double"), "doubleValue", 2)
  val Boolean = n("boolean", "Z", TypeBinding.BOOLEAN, Name("java.lang.Boolean"), "booleanValue")
  val Char = n("char", "C", TypeBinding.CHAR, Name("java.lang.Char"), "charValue")

  val allTypes = List(Byte, Short, Int, Long, Float, Double, Boolean, Char)
  def numericTypes = List(Byte, Short, Int, Long, Float, Double, Char)
  def widening(from: PrimitiveJavaType, to: PrimitiveJavaType) = {
    from match {
      case Byte  ⇒ List(Short, Int, Long, Float, Double).contains(to)
      case Short ⇒ List(Int, Long, Float, Double).contains(to)
      case Char  ⇒ List(Int, Long, Float, Double).contains(to)
      case Int   ⇒ List(Long, Float, Double).contains(to)
      case Long  ⇒ List(Float, Double).contains(to)
      case Float ⇒ to == Double
      case _     ⇒ false
    }
  }
  def toOperationType(t: PrimitiveJavaType): PrimitiveJavaType = {
    t match {
      case Byte   ⇒ Int
      case Short  ⇒ Int
      case Char   ⇒ Int
      case Int    ⇒ Int
      case Long   ⇒ Long
      case Float  ⇒ Float
      case Double ⇒ Double
    }
  }
  def getUnboxedType(p: ClassJavaType): Option[PrimitiveJavaType] = {
    p.name.str match {
      case "java.lang.Boolean" ⇒ Some(primitives.Boolean)
      case "java.lang.Char"    ⇒ Some(primitives.Char)
      case "java.lang.Byte"    ⇒ Some(primitives.Byte)
      case "java.lang.Short"   ⇒ Some(primitives.Short)
      case "java.lang.Integer" ⇒ Some(primitives.Int)
      case "java.lang.Float"   ⇒ Some(primitives.Float)
      case "java.lang.Double"  ⇒ Some(primitives.Double)
      case "java.lang.Long"    ⇒ Some(primitives.Long)
      case _                   ⇒ None
    }
  }
  /** must be int long float or double (operationtype) */
  def largerOperation(a: PrimitiveJavaType, b: PrimitiveJavaType): PrimitiveJavaType = {
    val l = List(Int, Long, Float, Double)
    l(math.max(l.indexOf(a), l.indexOf(b)))
  }
  def isNumeric(tpe: Type): Boolean = {
    tpe match {
      case p: PrimitiveJavaType if (p != primitives.Boolean) ⇒ true
      case j: JavaType ⇒ false
      case _ ⇒ false
    }
  }
  def isIntNumeric(tpe: Type): Boolean = tpe == primitives.Int ||
    tpe == primitives.Short ||
    tpe == primitives.Byte ||
    tpe == primitives.Char
  def find(desc: String) = allTypes.find(_.descriptor == desc)
  def find(name: Name) = allTypes.find(_.name == name)
}

trait ReporterAdapter {
  def location(tree: Tree): Int
  def reporter: Reporter
  def error(str: String, tree: Tree) = reporter.report(str, Some(location(tree)))
}
class Analyzer(val reporter: Reporter, val toCompile: BoxDef) {
  def globLocation(t: Tree) = t.line

  class Namer extends Traverser(null) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)

    private def bind(symbol: Symbol, tree: Tree, dupl: Boolean)(block: ⇒ Unit) {
      if (dupl) error("Duplicate symbol " + symbol.name, tree)
      tree.symbol = symbol
      symbol.decl = tree
      block
    }
    override def traverse(tree: Tree) {
      tree match {
        case b: BoxDef ⇒
          val cl = Some(Name(classOf[JPanel].getName))
          val sym = new BoxTypeSymbol(b.name, b.pkg, b.image, cl)
          sym.hasApply = true
          bind(sym, b, /*global.lookupBoxType(b.name).isDefined*/ false) {}
          sym.constructors = List(new Constructor(sym, List()))
          tree.tpe = sym
          if (b.template.blocks.size != 1) error("Fatal: BoxDef must have 1 block defined. Manual edit needed.", b) 
        // FIXME reported errors do not show in the editor (valdef)
        case t: Template ⇒
          val template = currentOwner.asInstanceOf[TemplateSymbol]
          t.symbol = template
        case b: Block ⇒
          val template = currentOwner.asInstanceOf[TemplateSymbol]
          val blockSym = new BlockSymbol(template)
          bind(blockSym, b, false) {
            template.blocks :+= blockSym
          }
        case p @ PortDef(name, typeName, dir, inPos, extPos) ⇒
          val template = currentOwner.asInstanceOf[TemplateSymbol]
          val port = new PortSymbol(template, name, None, extPos, dir)
          bind(port, p, template.ports.contains(p.name)) {
            template.ports += (port.name -> port)
          }
        case v: ValDef ⇒
          val block = currentOwner.asInstanceOf[BlockSymbol]
          val vs = new ValSymbol(block, v.name)
          bind(vs, v, block.vals.contains(v.name)) {
            block.vals += (v.name -> vs)
          }
        case _ ⇒
      }
      super.traverse(tree)
    }
  }
  var cud: ZaluumCompilationUnitDeclaration = _

  class Resolver(global: Scope) extends Traverser(global) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def createPortInstances(ports: Iterable[PortSymbol], vsym: ValSymbol, inside: Boolean, outside: Boolean) = {
      vsym.portInstances :::= (for (p ← ports; if p.isInstanceOf[PortSymbol]) yield {
        val pi = new PortInstance(p.name, p.helperName, vsym, p.dir, Some(p))
        pi
      }).toList;
      vsym.portSides :::= (for (pi ← vsym.portInstances; ps ← pi.portSymbol) yield {
          def define(fromInside: Boolean) = ps.dir match {
            case In    ⇒ List(new PortSide(pi, true, fromInside))
            case Out   ⇒ List(new PortSide(pi, false, fromInside))
            case Shift ⇒ List(new PortSide(pi, true, fromInside), new PortSide(pi, false, fromInside))
          }
        val i = if (inside) define(true) else List()
        val o = if (outside) define(false) else List()
        i ::: o
      }).flatMap(a ⇒ a);
    }
    private def catchAbort[T](b: ⇒ Option[T]): Option[T] =
      try { b } catch { case e: AbortCompilation ⇒ None }
    override def traverse(tree: Tree) {
      super.traverse(tree)
      tree match {
        case b: BoxDef ⇒
          val bs = b.sym
          bs.scope = global
          val fields = bs.ports.values.toList
            .filter { _.dir == Out }
            .sortBy { _.name.str }
            .drop(1)
          fields.foreach { _.isField = true }
          createPortInstances(bs.ports.values, bs.thisVal, true, false)
        case bl: Block ⇒
          bl.sym.template match {
            case bs: BoxTypeSymbol ⇒
              assert(bs.thisVal==null)
              bs.thisVal = new ValSymbol(bl.sym, Name("this")) // feels wrong
              bs.thisVal.decl = bl.sym.template.decl
              bs.thisVal.tpe = bs
            case v: ValSymbol ⇒
              v.thisVal = v
          }
        case p: PortDef ⇒
          tree.symbol.tpe = catchAbort(global.lookupType(p.typeName)) getOrElse {
            error("Port type \"" + p.typeName + "\" not found in port " + p.sym.name.str,
                tree); NoSymbol
          }
          tree.tpe = tree.symbol.tpe
        case v: ValDef ⇒
          catchAbort(Expressions.find(v.typeName) orElse global.lookupBoxType(v.typeName)) match {
            case Some(bs: BoxTypeSymbol) ⇒
              val vsym = v.sym.asInstanceOf[ValSymbol]
              v.symbol.tpe = bs
              if (!bs.hasApply) {
                error("Class " + v.typeName.str + " is not a annotated with @Box or has no method annotated with @Apply", tree)
              }
              // Constructor
              val consSign = v.constructorTypes map { name ⇒
                global.lookupType(name) getOrElse {
                  error("Constructor type " + name + " not found" , tree)
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
                        NoSymbol
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
                      NoSymbol
                    }
                    vsym.params += (parSym -> parsed)
                  case None ⇒ error(bs.name.str  + " has no parameter " + p.key.str, tree)
                }
              }
              createPortInstances(bs.ports.values, vsym, false, true)
              vsym.params
            case Some(b: ExprType) ⇒
              v.symbol.tpe = b
              val vsym = v.sym
              for (p ← v.params.asInstanceOf[List[Param]]) {
                b.lookupParam(p.key) match {
                  case Some(parSym) ⇒
                    vsym.params += (parSym -> p.value) // FIXME always string?
                  case None ⇒ error(b.fqName.str + " has no parameter " + p.key, tree)
                }
              }
              val createInside = b.isInstanceOf[TemplateExprType]
              createPortInstances(vsym.ports.values, vsym, createInside, true)
              createPortInstances(b.ports.values, vsym, createInside, true)
            case a ⇒
              v.symbol.tpe = NoSymbol
              error("Box class " + v.typeName + " not found", tree);
          }
          // constructor match
          tree.tpe = tree.symbol.tpe
        case ThisRef() ⇒ // 
          val block = currentOwner.asInstanceOf[BlockSymbol]
          tree.symbol = block.owner
          tree.tpe =  tree.symbol.tpe
        case _ ⇒
      }
    }
  }

  def runNamer() = new Namer().traverse(toCompile)

  def runResolve(cud: ZaluumCompilationUnitDeclaration, global: Scope): Tree = {
    this.cud = cud
    new Resolver(global).traverse(toCompile)
    toCompile
  }
  def runCheck() {
    reporter.ifErrorsDoNotReportMore()
    toCompile.template.blocks.headOption foreach {
      bl ⇒
        new CheckConnections(bl, true, this).run()
    }
  }
}
class CompilationException extends Exception
