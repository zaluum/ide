package org.zaluum.nide.compiler

import scala.collection.mutable.Buffer
import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.problem.AbortCompilation
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import org.zaluum.nide.eclipse.integration.model.ZaluumTypeDeclaration
import javax.swing.JPanel
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.utils.JDTUtils
import org.eclipse.jdt.internal.compiler.lookup.ProblemMethodBinding
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding

class Reporter {
  case class Error(msg: String, mark: Option[Int])
  private var reportMoreErrors = true
  def ifErrorsDoNotReportMore() {
    if (errors.size > 0) reportMoreErrors = false
  }
  val errors = Buffer[Error]()
  def report(str: String, mark: Option[Int] = None) {
    if (reportMoreErrors) errors += Error(str, mark)
  }
  override def toString = errors.toString
}
object Name {
  def apply(cl: Class[_]): Name = Name(cl.getName())
}
case class Name(str: String) {
  def classNameWithoutPackage = Name(str.split('.').last)
  def packageProxy = str.split('.').dropRight(1).mkString(".")
  def toRelativePath: String = str.replace('.', '/')
  def toRelativePathClass = toRelativePath + ".class"
  def internal = str.replace('.', '/')
  def firstLowerCase = str.head.toLower + str.tail
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

}
trait Scope extends Symbol {
  def lookupType(name: Name): Option[JavaType]
  def javaScope: ZaluumClassScope
}

trait ReporterAdapter {
  def location(tree: Tree): Int
  def reporter: Reporter
  def error(str: String, tree: Tree) = reporter.report(str, Some(location(tree)))
}
class Analyzer(val reporter: Reporter, val toCompile: BoxDef, val binding: ReferenceBinding, scope: ZaluumClassScope) {
  def globLocation(t: Tree) = t.line

  class Namer extends Traverser(null) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)

    private def bind[S >: Null <: DeclSymbol[T], T <: SymbolTree[S]](symbol: S, tree: T, dupl: Boolean)(block: ⇒ Unit) {
      if (dupl) error("Duplicate symbol " + symbol.name, tree)
      tree.symbol = symbol
      symbol.decl = tree
      block
    }
    override def traverse(tree: Tree) {
      tree match {
        case b: BoxDef ⇒
          val sym = new BoxSymbol(b.image, true, binding, scope)
          sym.source = Some(b.name.str + ".zaluum")
          sym.hasApply = true
          bind(sym, b, /*global.lookupBoxType(b.name).isDefined*/ false) {}
          sym.constructors = List(new ConstructorDecl(List()))
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
  var ztd: ZaluumTypeDeclaration = _

  class Resolver extends Traverser(scope) with ReporterAdapter {
    def reporter = Analyzer.this.reporter
    def location(tree: Tree) = globLocation(tree)
    def createPortInstances(ports: Iterable[PortSymbol], vsym: ValSymbol, inside: Boolean, outside: Boolean) = {
      vsym.portInstances :::= (for (p ← ports; if p.isInstanceOf[PortSymbol]) yield {
        new PortInstance(p.name, p.helperName, vsym, p.dir, Some(p))
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
          if (!b.constructor.isEmpty) {
            bs.constructors = List(
              new ConstructorDecl(
                b.constructor map { varDecl ⇒
                  val p = new ParamDecl(varDecl.name)
                  p.tpe = scope.lookupType(varDecl.tpeName).orElse {
                    error("Cannot find constructor parameter type " + varDecl.tpeName.str + " for parameter " + varDecl.name.str, b)
                    None
                  }
                  varDecl.symbol = p
                  p
                }))
          }
          b.initMethod foreach { im ⇒
            Signatures.findStaticMethodUID(im, scope) match {
              case Some(p: ProblemMethodBinding) ⇒ error("cannot find init method " + im, tree)
              case Some(p: MethodBinding) ⇒
                if (p.parameters.length == 1 && p.parameters(0).erasure().isCompatibleWith(bs.binding))
                  bs.initMethod = Some(p)
                else error("bad init parameter", tree);
              case _ ⇒ error("cannot find init method " + im, tree)
            }
          }
          val outfields = bs.ports.values.toList // minor aplabetically is return and method name
            .filter { _.dir == Out }
            .sortBy { _.name.str }
            .drop(1)
          outfields.foreach { _.isField = true }
          createPortInstances(bs.ports.values, bs.thisVal, true, false)
          bs.methodSelector = bs.returnPort.map { _.name }.getOrElse(Name(TreeToClass.defaultMethodName))
        case bl: Block ⇒
          bl.sym.template match {
            case bs: BoxSymbol ⇒
              assert(bs.thisVal == null)
              bs.thisVal = new ValSymbol(bl.sym, Name("this")) // feels wrong
            //bs.thisVal.decl = bl.sym.template.decl
            //bs.thisVal.tpe = bs
            case v: ValSymbol ⇒
              v.thisVal = v
          }
        case p: PortDef ⇒
          p.sym.tpe = catchAbort(scope.lookupType(p.typeName)) orElse {
            if (p.typeName.str != "") {
              error("Port type \"" + p.typeName + "\" not found in port " + p.sym.name.str,
                tree);
            }
            None
          }
        case v: ValDef ⇒
          catchAbort(Expressions.find(v.typeName)) match {
            case Some(b) ⇒
              v.sym.tpe = Some(b)
              val vsym = v.sym
              for (p ← v.params) {
                b.lookupExprParam(p.key) match {
                  case None ⇒
                    if (b != BoxExprType) // can be beanparameter
                      error(b.fqName.str + " has no parameter " + p.key, tree)
                  case _ ⇒
                }
              }
              val createInside = b.isInstanceOf[TemplateExprType]
              createPortInstances(vsym.ports.values, vsym, createInside, true)
              createPortInstances(b.ports.values, vsym, createInside, true)
            case a ⇒
              v.sym.tpe = None
              error("Box class " + v.typeName + " not found", tree);
          }
        // constructor match
        case t: ThisRef ⇒ // 
          val block = currentOwner.asInstanceOf[BlockSymbol]
          t.symbol = block.template
        case _ ⇒
      }
    }
  }

  def runNamer() = new Namer().traverse(toCompile)

  def runResolve(ztd: ZaluumTypeDeclaration): Tree = {
    this.ztd = ztd
    new Resolver().traverse(toCompile)
    toCompile
  }
  def runCheck() {
    reporter.ifErrorsDoNotReportMore()
    toCompile.template.blocks.headOption foreach {
      bl ⇒
        new CheckConnections(bl, true, this).run()
    }
    toCompile.template.blocks.headOption foreach {
      bl ⇒
        if (reporter.errors.isEmpty)
          new AnalyzerParallelism(bl.sym, this).run()
    }

  }
}

