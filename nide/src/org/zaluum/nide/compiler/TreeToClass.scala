package org.zaluum.nide.compiler

import javax.swing.JLabel
import org.zaluum.basic.RunnableBox
import org.zaluum.nide.eclipse.integration.model.ZaluumCompilationUnitScope

trait BinaryExpr extends Tree {
  def a: Tree
  def b: Tree
}
trait UnaryExpr extends Tree {
  def a: Tree
}

case class BoxClass(name: Name, superName: Name, contents: List[Tree]) extends Tree
case class FieldDef(name: Name, typeName: Name, annotation: Option[Name], priv: Boolean) extends Tree
case class New(typeName: Name, param: List[Tree], signature: String) extends Tree
case class ConstructorMethod(boxCreation: List[Tree]) extends Tree
case class Method(name: Name, signature: String, stats: List[Tree], locals: List[(String, String, Int)]) extends Tree
case class Assign(lhs: Ref, rhs: Tree) extends Tree

case class Not(a: Tree, t: PrimitiveJavaType) extends UnaryExpr
case class Minus(a: Tree, t: PrimitiveJavaType) extends UnaryExpr

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

case object This extends Tree
case object Pop extends Tree
case object NullConst extends Tree
trait Ref extends Tree
case class Select(a: Tree, b: Tree) extends Ref
case class LocalRef(id: Int, typeName: Name) extends Ref
case class FieldRef(id: Name, typeName: Name, fromClass: Name) extends Ref
case class Invoke(
  obj: Tree, meth: String, param: List[Tree],
  fromClass: Name, descriptor: String, interface: Boolean) extends Tree
case class InvokeStatic(meth: String, param: List[Tree], fromClass: Name, descriptor: String) extends Tree
case class Const(i: Any, constTpe: Type) extends Tree
case class Return(t: Tree) extends Tree
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

class TreeToClass(t: Tree, global: Scope, zaluumScope: ZaluumCompilationUnitScope) extends ReporterAdapter with ContentsToClass {
  val reporter = new Reporter // TODO fail reporter
  def location(t: Tree) = 0 // FIXMELocation(List(0))

  object rewrite {
    def apply(t: Tree) = t match {
      case b: BoxDef ⇒
        val tpe = b.tpe.asInstanceOf[BoxType]
        val bs = b.sym
        val baseFields = (bs.blocks.head.executionOrder ++ bs.ports.values).flatMap { field(_) }
        val fields = bs.visualClass map { vn ⇒
          FieldDef(Name("_widget"), vn, None, false) :: baseFields
        } getOrElse { baseFields }
        val baseMethods = List(cons(b), appl(b))
        BoxClass(
          tpe.fqName,
          // TODO check super-name
          b.superName getOrElse { Name(classOf[RunnableBox].getName) },
          baseMethods ++ fields)
    }
    def field(s: Symbol) = t match {
      case ps: PortSymbol ⇒
        val a = ps.dir match {
          case Out ⇒ classOf[org.zaluum.annotation.Out]
          case _ ⇒ classOf[org.zaluum.annotation.In]
        }
        Some(FieldDef(ps.name, ps.tpe.name, Some(Name(a.getName)), false))
      case vs: ValSymbol ⇒
        vs.tpe match {
          case bs: BoxTypeSymbol =>
            Some(FieldDef(vs.name, bs.fqName, None, true))
          case _ => None
        }
      case _ ⇒ None
    }
    def cons(b: BoxDef) = {
      val bs = b.sym
      val vals = bs.blocks.head.valsList
      // boxes
      val boxCreation: List[Tree] = vals flatMap {
        _ match {
          case vs: ValSymbol ⇒
            vs.tpe match {
              case tpe: BoxTypeSymbol ⇒
                val sig = vs.constructor.get.signature
                val values = for ((v, t) ← vs.constructorParams) yield {
                  Const(v, t)
                }
                Some(Assign(
                  Select(This, FieldRef(vs.name, tpe.fqName, bs.fqName)),
                  New(tpe.fqName, values, sig)))
              case _ ⇒ None
            }
        }
      }
      // params
      val params = vals collect { case vs: ValSymbol ⇒ (vs, vs.tpe) } flatMap {
        case (vs, valBs: BoxTypeSymbol) ⇒
          vs.params map {
            case (param, v) ⇒
              Invoke(
                Select(This, FieldRef(vs.name, valBs.fqName, bs.fqName)),
                param.name.str,
                List(Const(v, param.tpe)),
                valBs.fqName,
                "(" + param.tpe.asInstanceOf[JavaType].descriptor + ")V",
                interface = false) // FIXME not always JavaType
          }
        case _ => List()
      }
      // widgets
      val widgets = bs.visualClass map { vn ⇒
        val widgetCreation: List[Tree] = List(
          Assign(Select(This, FieldRef(widgetName, vn, bs.fqName)),
            New(vn, List(NullConst), "(Ljava/awt/LayoutManager;)V")),
          Invoke(
            Select(This, FieldRef(widgetName, vn, bs.fqName)),
            "setSize",
            List(Const(b.guiSize.map(_.w).getOrElse(100), primitives.Int),
              Const(b.guiSize.map(_.h).getOrElse(100), primitives.Int)),
            Name("javax.swing.JComponent"),
            "(II)V",
            interface = false))
        widgetCreation ++ createWidgets(bs, List(), b)
      }
      ConstructorMethod(boxCreation ++ params ++ widgets.toList.flatten)
    }

    val widgetName = Name("_widget")
    def fieldRef(v: ValSymbol) = {
      val tpe = v.tpe.asInstanceOf[BoxTypeSymbol]
      val ownertpe = v.owner.template.asInstanceOf[BoxTypeSymbol]
      FieldRef(v.name, tpe.fqName, ownertpe.fqName)
    }
    def selectPath(path: List[ValSymbol]): Tree = {
      path match {
        case Nil ⇒ This
        case v :: tail ⇒ Select(selectPath(tail), fieldRef(v))
      }
    }
    def createWidget(path: List[ValSymbol], mainBox: BoxDef): List[Tree] = {
      val vs = path.head
      val valDef = vs.decl.asInstanceOf[ValDef]
      vs.tpe match {
        case tpe: BoxTypeSymbol ⇒
          val mainTpe = mainBox.sym
          tpe.visualClass map { cl ⇒
            val widgetSelect = Select(selectPath(path), FieldRef(widgetName, cl, tpe.fqName))
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
                Select(This, FieldRef(widgetName, mainBox.sym.visualClass.get, mainTpe.fqName)),
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
              Select(This, FieldRef(widgetName, mainBox.sym.visualClass.get, mainTpe.fqName)),
              "add",
              List(ALoad(1)),
              Name("javax.swing.JComponent"), "(Ljava/awt/Component;)Ljava/awt/Component;",
              interface = false),
            Pop)
        case None ⇒ List()
      }
    }
    def createWidgets(bs: BoxTypeSymbol, path: List[ValSymbol], mainBox: BoxDef): List[Tree] = {
      bs.blocks.flatMap {
        _.declaredVals.values.toList flatMap {
          case v: ValSymbol ⇒
            v.tpe match {
              case tpe: BoxTypeSymbol ⇒
                createWidget(v :: path, mainBox)
              case _ ⇒ List()
            }
        }
      }
    }
  }
  def run() = {
    rewrite(t)
  }
}