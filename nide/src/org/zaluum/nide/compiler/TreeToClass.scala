package org.zaluum.nide.compiler

import org.zaluum.basic.RunnableBox
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitScope
import javax.swing.JLabel
import org.zaluum.nide.eclipse.integration.model.ZaluumClassScope
import scala.collection.mutable.Buffer

trait BinaryExpr extends Tree {
  def a: Tree
  def b: Tree
}
trait UnaryExpr extends Tree {
  def a: Tree
}
case class RunnableClass(fqName: Name, simpleName: Name, run: Method) extends Tree
case class BoxClass(name: Name, contents: List[Tree], superName: Name, inners: List[RunnableClass]) extends Tree
case class FieldDef(name: Name, typeName: Name, annotation: Option[Name], priv: Boolean) extends Tree
case class New(typeName: Name, param: List[Tree], signature: String) extends Tree
case class ConstructorMethod(boxCreation: List[Tree], superName: Name) extends Tree
case class Method(name: Name, signature: String, stats: List[Tree], locals: List[(String, String, Int)], applyAnnotation: Option[List[Name]]) extends Tree
case class Assign(lhs: Ref, rhs: Tree) extends Tree
case class While(body: List[Tree], cond: Tree) extends Tree
case class If(cond: Tree, trueBlock: List[Tree], falseBlock: List[Tree]) extends Tree
case class Not(a: Tree, t: PrimitiveJavaType) extends UnaryExpr
case class Minus(a: Tree, t: PrimitiveJavaType) extends UnaryExpr
case class ArrayRef(index: Tree, arrRef: Tree, arrTpe: JavaType) extends Ref
case class NewArray(sizes: List[Tree], arrTpe: JavaType) extends Ref

case class ShiftLeft(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class ShiftRight(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class UShiftRight(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr

case class And(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Or(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Xor(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr

case class Add(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Sub(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Mul(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Div(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Rem(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr

case class Lt(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Le(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Gt(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Ge(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Eq(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr
case class Ne(a: Tree, b: Tree, t: PrimitiveJavaType) extends BinaryExpr

case object This extends Ref
case object Pop extends Tree
case object NullConst extends Tree
trait Ref extends Tree
case class Select(a: Tree, b: Tree) extends Ref
case class LocalRef(id: Int, typeName: Name) extends Ref
case class FieldRef(id: Name, descriptor: String, fromClass: Name) extends Ref
case class FieldStaticRef(id: Name, descriptor: String, fromClass: Name) extends Ref
case class Invoke(
  obj: Tree, meth: String, param: List[Tree],
  fromClass: Name, descriptor: String, interface: Boolean) extends Tree
case class InvokeStatic(meth: String, param: List[Tree], fromClass: Name, descriptor: String) extends Tree
case class Const(i: Any, constTpe: Name) extends Tree {
  def this(i: Any, tpe: JavaType) = this(i, tpe.fqName)
}
case class Return(t: Tree, retTpe: Name) extends Tree
case object Return extends Tree
case object True extends Tree
case object Dup extends Tree
case class ALoad(i: Int) extends Tree
case class AStore(i: Int) extends Tree
case class I2B(a: Tree) extends UnaryExpr
case class I2C(a: Tree) extends UnaryExpr
case class I2D(a: Tree) extends UnaryExpr
case class I2F(a: Tree) extends UnaryExpr
case class I2L(a: Tree) extends UnaryExpr
case class I2S(a: Tree) extends UnaryExpr
case class F2D(a: Tree) extends UnaryExpr
case class F2I(a: Tree) extends UnaryExpr
case class F2L(a: Tree) extends UnaryExpr
case class D2F(a: Tree) extends UnaryExpr
case class D2I(a: Tree) extends UnaryExpr
case class D2L(a: Tree) extends UnaryExpr
case class L2D(a: Tree) extends UnaryExpr
case class L2F(a: Tree) extends UnaryExpr
case class L2I(a: Tree) extends UnaryExpr
object TreeToClass {
  val defaultMethodName = "run"
  val enclosingClassFieldName = "this$0"
  val runnableClassName = Name(classOf[java.lang.Runnable].getName)
  val semaphoreClassName = Name(classOf[java.util.concurrent.Semaphore].getName)
  val futureClassName = Name(classOf[java.util.concurrent.Future[_]].getName)
}

class TreeToClass(b: BoxDef, global: Scope, zaluumScope: ZaluumClassScope) extends ReporterAdapter with GeneratorHelpers {
  val bs = b.sym
  val block = bs.block
  val reporter = new Reporter // TODO fail reporter
  def location(t: Tree) = t.line
  object run {
    val fieldDecls = Buffer[FieldDef]()
    val fieldInits = Buffer[Tree]()
    def field(name: Name, tpe: Name, init: Option[Tree] = None, annotation: Option[Name] = None) {
      fieldDecls += FieldDef(name, tpe, annotation, false)
      init foreach { t ⇒
        fieldInits += Assign(Select(thisRef, FieldRef(name, tpe.descriptor, bs.fqName)), t)
      }
    }
    def apply() = {
      val enclosed = for (
        bl ← (block :: deepChildBlocks(block));
        ep ← bl.secondaryPaths
      ) yield {
        val method = new RunnableMethodGenerator(bs, ep)()
        RunnableClass(
          ep.fqName(bs),
          ep.name,
          method)
      }
      populateFields(bs)
      val baseMethods = List(cons(b), new MainThreadMethodGenerator(bs)())
      BoxClass(
        bs.tpe.fqName,
        baseMethods ++ fieldDecls,
        superName, enclosed)

    }

    /*
     * Fields:
     * 	* arguments needed by another thread
     *  * public second returns of boxtypesymbol  
     * 	* Widget
     *       *  
     *  for all valsymbols
     *  * valsymbol 
     *  * if (jointpoint) 
     *  	if (expression) -holder for all return values
     *      else -holder for first return value  
     *      -Semaphores
     *  * Threads
     *  
     *  * Cyclic barrier if needed
     */
    def populateFields(bs: BoxTypeSymbol) {
      // externals  (interface)
      bs.fieldReturns foreach { ps ⇒
        assert(ps.dir == Out && ps.isField)
        val outName = Name(classOf[org.zaluum.annotation.Out].getName)
        field(ps.name, ps.tpe.fqName, None, annotation = Some(outName))
      }
      // internals 
      // valsymbols
      deepChildValSymbols(block) foreach { vs ⇒
        // box instance
        vs.tpe match {
          case tbs: BoxTypeSymbol if (vs != bs.thisVal) ⇒
            val sig = vs.constructor.get.signature
            val values = for (v ← vs.constructorParams) yield v.codeGen
            val init = New(vs.tpe.fqName, values, sig)
            field(vs.fqName, tbs.fqName, init = Some(init)) // make private?
          case _ ⇒
        }
        // join points holder 
        for (pi ← vs.portInstances; if pi.internalStorage == StorageJoinField) yield {
          field(pi.joinfqName, pi.tpe.fqName)
        }
        // join semaphore
        if (vs.isJoinPoint) {
          field(vs.semfqName, TreeToClass.semaphoreClassName, init = Some(
            New(TreeToClass.semaphoreClassName, List(Const(0, primitives.Int.fqName)), "(I)V")))
        }
      }
      // thread end semaphore
      for (t ← secondThreads) {
        field(t.futureFqName, TreeToClass.futureClassName, init = None)
        field(t.name, t.fqName(bs), init = Some(
          New(t.fqName(bs), List(This), "(" + bs.tpe.fqName.descriptor + ")V")))
      }
    }

    lazy val secondThreads = (block :: deepChildBlocks(block)) flatMap { _.secondaryPaths }

    def cons(b: BoxDef) = {
      val bs = b.sym
      val bsVals = bs.blocks.head.valsAlphabeticOrder
        // params
        def params(vs: ValSymbol): List[Tree] = vs.tpe match {
          case tpe: BoxTypeSymbol ⇒
            vs.params map {
              case (param, v) ⇒
                val beanProp = tpe.beanProperties.find(_.name == param.name).get
                Invoke(
                  valRef(vs),
                  beanProp.setter.selector.mkString,
                  List(v.codeGen),
                  tpe.fqName,
                  "(" + param.tpe.descriptor + ")V",
                  interface = false)
            } toList
          case e: ExprType ⇒
            for (bl ← vs.blocks; vs ← bl.valsAlphabeticOrder; p ← params(vs)) yield p
        }
      // widgets
      val widgets = if (bs.isVisual) {
        val widgetCreation: List[Tree] = List(
          Invoke(This,
            "setLayout",
            List(NullConst),
            Name("java.awt.Container"),
            "(Ljava/awt/LayoutManager;)V",
            false),
          Invoke(
            This,
            "setPreferredSize",
            List(
              New(
                Name("java.awt.Dimension"),
                List(
                  Const(b.guiSize.map(_.w).getOrElse(100), primitives.Int.fqName),
                  Const(b.guiSize.map(_.h).getOrElse(100), primitives.Int.fqName)),
                "(II)V")),
            Name("javax.swing.JComponent"),
            "(Ljava/awt/Dimension;)V",
            interface = false))
        widgetCreation ++ bsVals.flatMap(createWidgets(_, bs.tdecl))
      } else List()
      val par = bsVals.flatMap(params)
      ConstructorMethod(fieldInits.toList ++ par ++ widgets :+ Return, superName)
    }
    def superName = if (bs.isVisual)
      Name("javax.swing.JPanel")
    else Name("java.lang.Object")

    def placeWidget(vs: ValSymbol, mainBox: BoxDef): List[Tree] = {
      vs.tpe match {
        case tpe: BoxTypeSymbol if tpe.isVisual ⇒
          val valDef = vs.tdecl
          val mainTpe = mainBox.sym
          List[Tree](
            Invoke(
              This,
              "add",
              List(valRef(vs)),
              Name("java.awt.Container"), "(Ljava/awt/Component;)Ljava/awt/Component;",
              interface = false),
            Pop) ++ createLabel(vs, mainBox)
        case _ ⇒ List()
      }
    }
    def createLabel(vs: ValSymbol, mainBox: BoxDef): List[Tree] = {
      val v = vs.decl.asInstanceOf[ValDef]
      val mainTpe = mainBox.sym
      v.labelGui match {
        case Some(lbl) ⇒
          val jlabel = new JLabel(lbl.description) // TODO better way to get size
          val jdim = jlabel.getPreferredSize

          val pos = v.sym.bounds.map { r ⇒ Point(r.x, r.y) }.getOrElse(Point(0, 0)) + lbl.pos + Vector2(0, -jdim.height);
          List[Tree](
            New(Name("javax.swing.JLabel"), List(Const(lbl.description, zaluumScope.getZJavaLangString.fqName)), "(Ljava/lang/String;)V"),
            AStore(1),
            Invoke(
              ALoad(1),
              "setBounds",
              List(Const(pos.x, primitives.Int.fqName),
                Const(pos.y, primitives.Int.fqName),
                Const(jdim.width, primitives.Int.fqName),
                Const(jdim.height, primitives.Int.fqName)),
              Name("java.awt.Component"),
              "(IIII)V",
              interface = false),
            Invoke(
              This,
              "add",
              List(ALoad(1)),
              Name("java.awt.Container"), "(Ljava/awt/Component;)Ljava/awt/Component;",
              interface = false),
            Pop)
        case None ⇒ List()
      }
    }
    def createWidgets(vs: ValSymbol, mainBox: BoxDef): List[Tree] = {
      vs.tpe match {
        case tpe: BoxTypeSymbol ⇒ placeWidget(vs, mainBox)
        case e: ExprType ⇒
          for (bl ← vs.blocks; vs ← bl.valsAlphabeticOrder; w ← createWidgets(vs, mainBox)) yield w
      }
    }
  }
}
trait GeneratorHelpers {
  def bs: BoxTypeSymbol
  def thisRef: Ref = This
  def threadRef(startPath: ExecutionPath) =
    Select(thisRef, FieldRef(startPath.name, startPath.fqName(bs).descriptor, bs.fqName))
  def valRef(vs: ValSymbol): Ref = {
    if (vs == bs.thisVal) thisRef
    else Select(thisRef, FieldRef(vs.fqName, vs.tpe.fqName.descriptor, bs.fqName))
  }
  def semaphoreRef(vs: ValSymbol) =
    Select(thisRef, FieldRef(vs.semfqName, TreeToClass.semaphoreClassName.descriptor, bs.fqName))

  def futureRef(t: ExecutionPath) =
    Select(thisRef, FieldRef(t.futureFqName, TreeToClass.futureClassName.descriptor, bs.fqName))

  def deepChildValSymbols(bl: BlockSymbol): List[ValSymbol] =
    bl.valsAlphabeticOrder flatMap { vs ⇒
      vs :: deepChildValSymbols(vs)
    }
  def deepChildBlocks(bl: BlockSymbol): List[BlockSymbol] =
    bl.valsAlphabeticOrder filter (_.tpe.isInstanceOf[ExprType]) flatMap { vs ⇒
      vs.blocks flatMap { bl ⇒
        bl :: deepChildBlocks(bl)
      }
    }
  def deepChildMainPaths(exec: ExecutionPath): List[ExecutionPath] = {
    exec.instructions flatMap { vs ⇒
      vs.tpe match {
        case bs: BoxTypeSymbol ⇒ List()
        case et: ExprType ⇒
          val mainPaths = vs.blocks.map { _.mainPath }
          mainPaths ::: (mainPaths flatMap { th ⇒ deepChildMainPaths(th) })
      }
    }
  }
  def deepChildValSymbolsThread0(vs: ValSymbol): List[ValSymbol] = {
    vs.tpe match {
      case bs: BoxTypeSymbol if (bs.thisVal == vs) ⇒
        bs.blocks flatMap (bl ⇒
          bl.execPaths(0).instructions flatMap { vs ⇒
            deepChildValSymbolsThread0(vs)
          })
      case bs: BoxTypeSymbol ⇒ List()
      case et: ExprType ⇒
        vs.blocks flatMap (vs ⇒ deepChildValSymbols(vs))
    }
  }
  def deepChildValSymbols(vs: ValSymbol): List[ValSymbol] = {
    vs.tpe match {
      case bs: BoxTypeSymbol if (bs.thisVal == vs) ⇒
        bs.blocks flatMap (bl ⇒ deepChildValSymbols(bl))
      case bs: BoxTypeSymbol ⇒ List()
      case et: ExprType ⇒
        vs.blocks flatMap (vs ⇒ deepChildValSymbols(vs))
    }
  }
}