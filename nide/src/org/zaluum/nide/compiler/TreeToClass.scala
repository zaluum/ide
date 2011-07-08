package org.zaluum.nide.compiler

import javax.swing.JLabel
import org.zaluum.basic.RunnableBox

case class BoxClass(name: Name, superName: Name, contents: List[Tree]) extends Tree
case class FieldDef(name: Name, typeName: Name, annotation: Option[Name], priv: Boolean) extends Tree
case class New(typeName: Name, param: List[Tree], signature: String) extends Tree
case class ConstructorMethod(boxCreation: List[Tree]) extends Tree
case class Method(name: Name, signature: String, stats: List[Tree], locals: List[(String, String, Int)]) extends Tree
case class Assign(lhs: Tree, rhs: Tree) extends Tree
case class Sum(a: Tree, b: Tree) extends Tree
case class Select(a: Tree, b: Tree) extends Tree
case object This extends Tree
case object Pop extends Tree
case object NullConst extends Tree
case class LocalRef(id: Int, typeName: Name) extends Tree
case class FieldRef(id: Name, typeName: Name, fromClass: Name) extends Tree
case class Invoke(obj: Tree, meth: String, param: List[Tree], fromClass: Name, descriptor: String) extends Tree
case class Const(i: Any) extends Tree
case class Return(t: Tree) extends Tree
case object True extends Tree
case object Dup extends Tree
case class ALoad(i: Int) extends Tree
case class AStore(i: Int) extends Tree
class TreeToClass(t: Tree, global: Scope) extends ReporterAdapter {
  val reporter = new Reporter // TODO fail reporter
  def location(t: Tree) = 0 // FIXMELocation(List(0))
  object orderValDefs extends CopyTransformer with CopySymbolTransformer {
    val trans: PartialFunction[Tree, Tree] = {
      case b: BoxDef ⇒
        val orderVals = b.symbol.asInstanceOf[BoxTypeSymbol].executionOrder map { _.decl }
        atOwner(b.symbol) {
          b.copy(
            defs = transformTrees(b.defs),
            vals = transformTrees(orderVals),
            ports = transformTrees(b.ports),
            connections = transformTrees(b.connections),
            junctions = transformTrees(b.junctions))
        }
    }
  }
  object rewrite {
    def vClass(bd: BoxDef): Option[Name] = {
      bd.symbol.asInstanceOf[BoxTypeSymbol].visualClass
    }
    def apply(t: Tree) = t match {
      case b: BoxDef ⇒
        val tpe = b.tpe.asInstanceOf[BoxTypeSymbol]
        val baseFields = (b.vals ++ b.ports).flatMap { field(_) }
        val fields = vClass(b) map { vn ⇒
          FieldDef(Name("_widget"), vn, None, false) :: baseFields
        } getOrElse { baseFields }
        val baseMethods = List(cons(b), appl(b))
        BoxClass(
          tpe.fqName,
          // TODO check super-name
          b.superName getOrElse { Name(classOf[RunnableBox].getName) },
          baseMethods ++ fields)
    }
    def field(t: Tree) = t match {
      case PortDef(name, typeName, dir, inPos, extPos) ⇒
        val a = dir match {
          case Out ⇒ classOf[org.zaluum.annotation.Out]
          case _ ⇒ classOf[org.zaluum.annotation.In]
        }
        Some(FieldDef(name, t.symbol.tpe.name, Some(Name(a.getName)), false))
      case v: ValDef if (v.symbol.tpe.isInstanceOf[BoxTypeSymbol]) ⇒
        val tpe = v.symbol.tpe.asInstanceOf[BoxTypeSymbol]
        Some(FieldDef(v.name, t.symbol.tpe.asInstanceOf[BoxTypeSymbol].fqName, None, true))
      case _ ⇒ None
    }
    def cons(b: BoxDef) = {
      val bs = b.symbol.asInstanceOf[BoxTypeSymbol]
      // boxes
      val boxCreation: List[Tree] = b.vals flatMap {
        _ match {
          case v: ValDef ⇒
            v.symbol.tpe match {
              case tpe: BoxTypeSymbol ⇒
                val vs = v.symbol.asInstanceOf[ValSymbol]
                val sig = vs.constructor.get.signature
                val values = for ((v, t) ← vs.constructorParams) yield {
                  Const(v)
                }
                Some(Assign(
                  Select(This, FieldRef(v.name, tpe.fqName, bs.fqName)),
                  New(tpe.fqName, values, sig)))
              case _ ⇒ None
            }
        }
      }
      // params
      val params = b.vals collect { case v: ValDef ⇒ (v, v.symbol.tpe) } flatMap {
        case (valDef, valBs: BoxTypeSymbol) ⇒
          val valSym = valDef.symbol.asInstanceOf[ValSymbol]
          valSym.params map {
            case (param, v) ⇒
              Invoke(
                Select(This, FieldRef(valSym.name, valBs.fqName, bs.fqName)),
                param.name.str,
                List(Const(v)),
                valBs.fqName,
                "(" + param.tpe.asInstanceOf[JavaType].descriptor + ")V")
          }
        case _ => List()
      }
      // widgets
      val widgets = vClass(b) map { vn ⇒
        val widgetCreation: List[Tree] = List(
          Assign(Select(This, FieldRef(widgetName, vn, bs.fqName)),
            New(vn, List(NullConst), "(Ljava/awt/LayoutManager;)V")),
          Invoke(
            Select(This, FieldRef(widgetName, vn, bs.fqName)),
            "setSize",
            List(Const(b.guiSize.map(_.w).getOrElse(100)),
              Const(b.guiSize.map(_.h).getOrElse(100))),
            Name("javax.swing.JComponent"),
            "(II)V"))
        widgetCreation ++ createWidgets(bs, List(), b)
      }
      ConstructorMethod(boxCreation ++ params ++ widgets.toList.flatten)
    }

    val widgetName = Name("_widget")
    def fieldRef(v: ValSymbol) = {
      val tpe = v.tpe.asInstanceOf[BoxTypeSymbol]
      val ownertpe = v.owner.asInstanceOf[BoxTypeSymbol]
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
          val mainTpe = mainBox.symbol.asInstanceOf[BoxTypeSymbol]
          tpe.visualClass map { cl ⇒
            val widgetSelect = Select(selectPath(path), FieldRef(widgetName, cl, tpe.fqName))
            List[Tree](
              Invoke(
                widgetSelect,
                "setBounds",
                List(Const(valDef.guiPos.map(_.x).getOrElse(0)),
                  Const(valDef.guiPos.map(_.y).getOrElse(0)),
                  Const(valDef.guiSize.map(_.w).getOrElse(50)),
                  Const(valDef.guiSize.map(_.h).getOrElse(50))),
                Name("javax.swing.JComponent"),
                "(IIII)V"),
              Invoke(
                Select(This, FieldRef(widgetName, vClass(mainBox).get, mainTpe.fqName)),
                "add",
                List(widgetSelect),
                Name("javax.swing.JComponent"), "(Ljava/awt/Component;)Ljava/awt/Component;"),
              Pop) ++ createLabel(vs, mainBox)
          } getOrElse List()
        case _ ⇒ List()
      }
    }
    def createLabel(vs: ValSymbol, mainBox: BoxDef): List[Tree] = {
      val v = vs.decl.asInstanceOf[ValDef]
      val mainTpe = mainBox.symbol.asInstanceOf[BoxTypeSymbol]
      v.labelGui match {
        case Some(lbl) ⇒
          val jlabel = new JLabel(lbl.description) // TODO better way to get size
          val jdim = jlabel.getPreferredSize
          val pos = v.guiPos.getOrElse(Point(0, 0)) + lbl.pos + Vector2(0, -jdim.height);
          List[Tree](
            New(Name("javax.swing.JLabel"), List(Const(lbl.description)), "(Ljava/lang/String;)V"),
            AStore(1),
            Invoke(
              ALoad(1),
              "setBounds",
              List(Const(pos.x),
                Const(pos.y),
                Const(jdim.width),
                Const(jdim.height)),
              Name("javax.swing.JComponent"),
              "(IIII)V"),
            Invoke(
              Select(This, FieldRef(widgetName, vClass(mainBox).get, mainTpe.fqName)),
              "add",
              List(ALoad(1)),
              Name("javax.swing.JComponent"), "(Ljava/awt/Component;)Ljava/awt/Component;"),
            Pop)
        case None ⇒ List()
      }
    }
    def createWidgets(b: BoxTypeSymbol, path: List[ValSymbol], mainBox: BoxDef): List[Tree] = {
      b.declaredVals.values.toList flatMap {
        case v: ValSymbol ⇒
          v.tpe match {
            case tpe: BoxTypeSymbol ⇒
              if (tpe.isLocal)
                createWidgets(tpe, v :: path, mainBox)
              else
                createWidget(v :: path, mainBox)
            case _ ⇒ List()
          }
      }
    }
    def appl(b: BoxDef): Method = {
      val bs = b.symbol.asInstanceOf[BoxTypeSymbol]
      // create locals for expressions
      var locals = 1; // 0 for "this"
      println (bs.valsInOrder)
      val localsMap = bs.valsInOrder flatMap { v =>
        v.tpe match {
          case b: BoxTypeSymbol => List()
          case s: SumExprType =>
            v.portSides map {
              (_ -> { locals = locals + 1; locals })
            }
          case _ => List()
        }
      } toMap;
      def toRef(p: PortSide): Tree = 
          if (p.fromInside) {
            Select(This, FieldRef(p.pi.portSymbol.name, p.pi.portSymbol.tpe.name, bs.fqName))
          } else {
            val vfrom = p.pi.valSymbol
            val ps = p.pi.portSymbol
            vfrom.tpe match {
              case b: BoxTypeSymbol =>
                Select(toRefVal(vfrom), FieldRef(ps.name, ps.tpe.name, vfrom.tpe.asInstanceOf[BoxTypeSymbol].fqName))
              case _ =>
                LocalRef(localsMap(p), Name("int"))
            }
          }
      def toRefVal(v:ValSymbol) = Select(This, FieldRef(v.name, v.tpe.asInstanceOf[BoxTypeSymbol].fqName, bs.fqName))
      // propagate initial inputs
      def execConnection(c: (PortSide, Set[PortSide])) = {
        val (out, ins) = c
        ins.toList map { in ⇒
          Assign(toRef(in), toRef(out))
        }
      }
      def connections = bs.connections
      def propagateInitialInputs = {
        val initialConnections = {
          connections.flow collect {
            case c @ (a: PortSide, _) ⇒ c
          } toList
        }
        initialConnections flatMap { execConnection(_) }
      }
      // execute in order
      def runOne(v: ValDef) = {
        val vs =v.symbol.asInstanceOf[ValSymbol]
        def outConnections = connections.flow collect {
          case c@(ps,_) if (ps.pi.valSymbol == vs) ⇒ c
        } toList
        val outs = outConnections flatMap { execConnection(_) }
        val invoke = v.tpe match {
          case bs: BoxTypeSymbol =>
            val tpe = v.tpe.asInstanceOf[BoxTypeSymbol].fqName
            Invoke(
              Select(This, FieldRef(v.name, tpe, bs.fqName)),
              "apply",
              List(),
              tpe,
              "()V")
          case s: SumExprType =>
            val vs = v.symbol.asInstanceOf[ValSymbol]
            def toPs(p:PortSymbol) = vs.findPortSide(p).get
            Assign(toRef(toPs(s.c)), Sum(toRef(toPs(s.a)), toRef(toPs(s.b))))
        }
        invoke :: outs
      }
      val invokes = b.vals flatMap { case v: ValDef ⇒ runOne(v) }
      val localsDecl = localsMap map { case (a, i) => (a.pi.portSymbol.name.str, "I", i) } toList;
      Method(Name("contents"), "()V", propagateInitialInputs ++ invokes, localsDecl)

    }
  }
  def run() = {
    val owner = global.root
    val mutated = orderValDefs(t, owner)
    //PrettyPrinter.print(mutated, 0)
    rewrite(mutated)
  }
}