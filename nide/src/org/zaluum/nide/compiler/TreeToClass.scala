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
case class BoxClass(name: Name, contents: List[Tree], inners: List[RunnableClass]) extends Tree
case class FieldDef(name: Name, typeName: Name, annotation: Option[Name], priv: Boolean) extends Tree
case class New(typeName: Name, param: List[Tree], signature: String) extends Tree
case class ConstructorMethod(boxCreation: List[Tree]) extends Tree
case class Method(name: Name, signature: String, stats: List[Tree], locals: List[(String, String, Int)], applyAnnotation: Option[List[Name]]) extends Tree
case class Assign(lhs: Ref, rhs: Tree) extends Tree
case class While(body: List[Tree], cond: Tree) extends Tree
case class If(cond: Tree, trueBlock: List[Tree], falseBlock: List[Tree]) extends Tree
case class Not(a: Tree, t: PrimitiveJavaType) extends UnaryExpr
case class Minus(a: Tree, t: PrimitiveJavaType) extends UnaryExpr
case class ArrayRef(index: Tree, arrRef: Tree, arrTpe: Type) extends Ref
case class NewArray(sizes: List[Tree], arrTpe: Type) extends Ref

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
case class Const(i: Any, constTpe: Type) extends Tree
case class Return(t: Tree, retTpe: Type) extends Tree
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
}

trait GeneratorHelpers {
  def bs: BoxTypeSymbol
  def thisRef: Ref = This
  def threadRef(t: ZThread) =
    Select(thisRef, FieldRef(t.name, t.fqName(bs).descriptor, bs.fqName))
  def valRef(vs: ValSymbol): Ref = {
    if (vs == bs.thisVal) thisRef
    else Select(thisRef, FieldRef(vs.fqName, vs.tpe.fqName.descriptor, bs.fqName))
  }
  def semaphoreRef(vs: ValSymbol) =
    Select(thisRef, FieldRef(vs.semfqName, TreeToClass.semaphoreClassName.descriptor, bs.fqName))
  def endSemaphoreRef(t: ZThread) =
    Select(thisRef, FieldRef(t.semFqName, TreeToClass.semaphoreClassName.descriptor, bs.fqName))

  def deepValSymbols(bl: BlockSymbol): List[ValSymbol] =
    bl.template.thisVal :: (
      for (vs ← bl.executionOrder; c ← deepValSymbols(vs)) yield c)
  def deepBlocks(bl: BlockSymbol): List[BlockSymbol] =
    bl :: (for (
      vs ← bl.executionOrder;
      if (vs.tpe.isInstanceOf[ExprType]);
      nbl ← vs.blocks;
      res ← deepBlocks(nbl)
    ) yield res)
  def deepValSymbols(vs: ValSymbol): List[ValSymbol] = {
    vs :: (vs.tpe match {
      case bs: BoxTypeSymbol if (bs.thisVal == vs) ⇒
        for (
          bl ← bs.blocks;
          vs ← deepValSymbols(bl)
        ) yield vs
      case bs: BoxTypeSymbol ⇒ List()
      case et: ExprType ⇒
        for (
          bl ← vs.blocks;
          vs ← deepValSymbols(bl)
        ) yield vs
    })
  }
  val widgetName = Name("_widget")
}
class TreeToClass(b: BoxDef, global: Scope, zaluumScope: ZaluumClassScope) extends ReporterAdapter with GeneratorHelpers {
  val bs = b.sym
  val block = bs.block
  val reporter = new Reporter // TODO fail reporter
  def location(t: Tree) = 0 // FIXMELocation(List(0))
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
        bl ← deepBlocks(block);
        t ← bl.threads.drop(1)
      ) yield {
        val method = new RunnableMethodGenerator(bs, t)()
        RunnableClass(
          t.fqName(bs),
          t.name,
          method)
      }
      populateFields(bs)
      val main = block.threads(0)
      val toSpawn = block.threads.drop(1) filter { _.forkedBy == None } toList
      val baseMethods = List(cons(b), new MainThreadMethodGenerator(bs, main, toSpawn, secondThreads)())
      BoxClass(
        bs.tpe.fqName,
        baseMethods ++ fieldDecls,
        enclosed)

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
        field(ps.name, ps.tpe.name, None, annotation = Some(outName))
      }
      // internals 
      //widget 
      bs.visualClass foreach { vn ⇒
        field(Name("_widget"), vn)
      }
      // valsymbols
      deepValSymbols(bs.blocks.head) foreach { vs ⇒
        vs.tpe match {
          case tbs: BoxTypeSymbol if (vs != bs.thisVal) ⇒
            val sig = vs.constructor.get.signature
            val values = for ((v, t) ← vs.constructorParams) yield {
              Const(v, t)
            }
            val init = New(vs.tpe.fqName, values, sig)
            field(vs.fqName, tbs.fqName, init = Some(init)) // make private?
          case _ ⇒
        }
        // join points holder and semaphore
        for (pi ← vs.portInstances; if pi.internalStorage == StorageJoinField) yield {
          field(pi.joinfqName, pi.tpe.fqName)
        }
        if (vs.isJoinPoint) {
          field(vs.semfqName, TreeToClass.semaphoreClassName, init = Some(
            New(TreeToClass.semaphoreClassName, List(Const(0, primitives.Int)), "(I)V")))
        }
      }
      // thread end semaphore
      for (t ← secondThreads) {
        field(t.semFqName, TreeToClass.semaphoreClassName, init = Some(
          New(TreeToClass.semaphoreClassName, List(Const(0, primitives.Int)), "(I)V")))
        field(t.name, t.fqName(bs), init = Some(
          New(t.fqName(bs), List(This), "(" + bs.tpe.fqName.descriptor + ")V")))
      }
    }

    lazy val secondThreads = deepBlocks(bs.blocks.head) flatMap { bl ⇒ bl.threads.drop(1) }

    def cons(b: BoxDef) = {
      val bs = b.sym
      val bsVals = bs.blocks.head.executionOrder
        // params
        def params(vs: ValSymbol): List[Tree] = vs.tpe match {
          case tpe: BoxTypeSymbol ⇒
            vs.params map {
              case (param, v) ⇒
                Invoke(
                  valRef(vs),
                  param.name.str,
                  List(Const(v, param.tpe)),
                  tpe.fqName,
                  "(" + param.tpe.asInstanceOf[JavaType].descriptor + ")V",
                  interface = false) // FIXME not always JavaType
            } toList
          case e: ExprType ⇒
            for (bl ← vs.blocks; vs ← bl.executionOrder; p ← params(vs)) yield p
        }
      // widgets
      val widgets = bs.visualClass.toList flatMap { vn ⇒
        val widgetCreation: List[Tree] = List(
          Assign(Select(This, FieldRef(widgetName, vn.descriptor, bs.fqName)),
            New(vn, List(NullConst), "(Ljava/awt/LayoutManager;)V")),
          Invoke(
            Select(This, FieldRef(widgetName, vn.descriptor, bs.fqName)),
            "setSize",
            List(Const(b.guiSize.map(_.w).getOrElse(100), primitives.Int),
              Const(b.guiSize.map(_.h).getOrElse(100), primitives.Int)),
            Name("javax.swing.JComponent"),
            "(II)V",
            interface = false))
        widgetCreation ++ bsVals.flatMap(createWidgets(_, bs.tdecl))
      }
      val par = bsVals.flatMap(params)
      ConstructorMethod(fieldInits.toList ++ par ++ widgets :+ Return)
    }
    def createWidget(vs: ValSymbol, mainBox: BoxDef): List[Tree] = {
      vs.tpe match {
        case tpe: BoxTypeSymbol ⇒
          val valDef = vs.tdecl
          val mainTpe = mainBox.sym
          tpe.visualClass map { cl ⇒
            val widgetSelect = Select(valRef(vs), FieldRef(widgetName, cl.descriptor, tpe.fqName))
            List[Tree](
              Invoke(
                widgetSelect,
                "setBounds",
                List(Const(valDef.guiPos.map(_.x).getOrElse(0), primitives.Int),
                  Const(valDef.guiPos.map(_.y).getOrElse(0), primitives.Int),
                  Const(valDef.guiSize.map(_.w).getOrElse(50), primitives.Int),
                  Const(valDef.guiSize.map(_.h).getOrElse(50), primitives.Int)),
                Name("javax.swing.JComponent"),
                "(IIII)V",
                interface = false),
              Invoke(
                Select(This, FieldRef(widgetName, mainBox.sym.visualClass.get.descriptor, mainTpe.fqName)),
                "add",
                List(widgetSelect),
                Name("javax.swing.JComponent"), "(Ljava/awt/Component;)Ljava/awt/Component;",
                interface = false),
              Pop) ++ createLabel(vs, mainBox)
          } getOrElse List()
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
          val pos = v.guiPos.getOrElse(Point(0, 0)) + lbl.pos + Vector2(0, -jdim.height);
          List[Tree](
            New(Name("javax.swing.JLabel"), List(Const(lbl.description, zaluumScope.getZJavaLangString)), "(Ljava/lang/String;)V"),
            AStore(1),
            Invoke(
              ALoad(1),
              "setBounds",
              List(Const(pos.x, primitives.Int),
                Const(pos.y, primitives.Int),
                Const(jdim.width, primitives.Int),
                Const(jdim.height, primitives.Int)),
              Name("javax.swing.JComponent"),
              "(IIII)V",
              interface = false),
            Invoke(
              Select(This, FieldRef(widgetName, mainBox.sym.visualClass.get.descriptor, mainTpe.fqName)),
              "add",
              List(ALoad(1)),
              Name("javax.swing.JComponent"), "(Ljava/awt/Component;)Ljava/awt/Component;",
              interface = false),
            Pop)
        case None ⇒ List()
      }
    }
    def createWidgets(vs: ValSymbol, mainBox: BoxDef): List[Tree] = {
      vs.tpe match {
        case tpe: BoxTypeSymbol ⇒ createWidget(vs, mainBox)
        case e: ExprType ⇒
          for (bl ← vs.blocks; vs ← bl.executionOrder; w ← createWidgets(vs, mainBox)) yield w
      }
    }
  }
}