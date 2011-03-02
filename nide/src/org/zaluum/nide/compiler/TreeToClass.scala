package org.zaluum.nide.compiler

import org.zaluum.runtime.RunnableBox

case class BoxClass(name: Name, superName:Name, contents : List[Tree]) extends Tree
case class FieldDef(name: Name, typeName: Name) extends Tree
case class New(typeName: Name, param: Tree, signature: String) extends Tree
case class ConstructorMethod(boxCreation: List[Tree]) extends Tree
case class Method(name: Name, signature:String, stats: List[Tree]) extends Tree
case class Assign(lhs: Tree, rhs: Tree) extends Tree
case class Select(a: Tree, b: Tree) extends Tree
case object This extends Tree
case object Pop extends Tree
case object NullConst extends Tree
case class FieldRef(id: Name, typeName: Name, fromClass: Name) extends Tree
case class Invoke(obj: Tree, meth: String, param: List[Tree], fromClass: Name, descriptor: String) extends Tree
case class Const(i: Any) extends Tree
case class Return(t:Tree) extends Tree
case object True extends Tree

class TreeToClass(t: Tree, global: Scope) extends ConnectionHelper with ReporterAdapter {
  val reporter = new Reporter // TODO fail reporter
  def location(t: Tree) = Location(List(0))
  object swapConnections extends CopyTransformer with CopySymbolTransformer {
    val trans: PartialFunction[Tree, Tree] = {
      case c: ConnectionDef ⇒
        val (from, to) = direction(c)
        ConnectionDef(transform(from), transform(to))
    }
  }
  object orderValDefs extends CopyTransformer with CopySymbolTransformer {
    val trans: PartialFunction[Tree, Tree] = {
      case b@BoxDef(name, superName, image, defs, vals, ports, connections) ⇒
        val orderVals = b.symbol.asInstanceOf[BoxTypeSymbol].executionOrder map { _.decl }
        atOwner(b.symbol) {
          BoxDef(name, superName, image,
            transformTrees(defs),
            transformTrees(orderVals),
            transformTrees(ports),
            transformTrees(connections))
        }
    }
  }
  object rewrite {
    def vClass(bd: BoxDef) : Option[Name]= {
      bd.symbol.asInstanceOf[BoxTypeSymbol].visualClass
    }
    def apply(t: Tree) = t match {
      case b@BoxDef(name, superName, image, defs, vals, ports, connections) ⇒
        val tpe = b.symbol.asInstanceOf[BoxTypeSymbol]
        val baseFields = (vals ++ ports).map { field(_) }
        val fields = vClass(b) map { vn =>
          FieldDef(Name("_widget"), vn) :: baseFields} getOrElse { baseFields }
        val baseMethods = List(cons(b), appl(b))
        BoxClass(
          tpe.fqName,
          // TODO check super-name
          superName getOrElse {Name(classOf[RunnableBox].getName)},
          baseMethods++ fields)
    }
    def field(t: Tree) = t match {
      case PortDef(name, typeName, dir, inPos, extPos) ⇒
        FieldDef(name, t.symbol.tpe.name)
      case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) ⇒
        val tpe = v.symbol.tpe.asInstanceOf[BoxTypeSymbol]
        FieldDef(name, t.symbol.tpe.asInstanceOf[BoxTypeSymbol].fqName)
    }
    def cons(b: BoxDef) = {
      val bs = b.symbol.asInstanceOf[BoxTypeSymbol]
      // boxes
      val boxCreation: List[Tree] = b.vals map {
        _ match {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) ⇒
            val tpe = v.symbol.tpe.asInstanceOf[BoxTypeSymbol]
            Assign(Select(This, FieldRef(name, tpe.fqName, bs.fqName)), New(tpe.fqName, EmptyTree, "()V"))
        }
      }
      // ports
      val ports = b.vals flatMap {
        case v:ValDef => 
          val valSym = v.symbol.asInstanceOf[ValSymbol]
          val valTpe = valSym.tpe.asInstanceOf[BoxTypeSymbol]
          valSym.params map {  
            case (param,v) =>
              Assign(
                Select(
                  Select(This,FieldRef(valSym.name,valTpe.fqName,bs.fqName)), 
                  FieldRef(param.name, param.tpe.name, valTpe.fqName) // TODO FIXME
                ),  
                Const(v)    
              )       
          }
      }
      // widgets
      val widgets = vClass(b) map { vn => 
        val widgetName = Name("_widget")
        val widgetCreation: Tree =
          Assign(Select(This, FieldRef(widgetName, vn, bs.fqName)),
            New(vn, NullConst, "(Ljava/awt/LayoutManager;)V"))
        val widgetInit = b.vals flatMap {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize,params) ⇒
            val tpe = v.tpe.asInstanceOf[BoxTypeSymbol];
            tpe.visualClass map { cl ⇒
              List[Tree](
                Invoke(
                  Select(Select(This, FieldRef(v.name, tpe.fqName, bs.fqName)),
                    FieldRef(widgetName, cl, tpe.fqName)),
                  "setBounds",
                  List(Const(guiPos.map(_.x).getOrElse(0)),
                    Const(guiPos.map(_.y).getOrElse(0)),
                    Const(guiSize.map(_.w).getOrElse(50)),
                    Const(guiSize.map(_.h).getOrElse(50))),
                  Name("javax.swing.JComponent"),
                  "(IIII)V"),
                Invoke(
                  Select(This, FieldRef(widgetName, vn, bs.fqName)),
                  "add",
                  List(Select(Select(This, FieldRef(v.name, tpe.fqName, bs.fqName)),
                    FieldRef(widgetName, cl, tpe.fqName))),
                  Name("javax.swing.JComponent"), "(Ljava/awt/Component;)Ljava/awt/Component;"),
                Pop)
            } getOrElse (List())
        }
        widgetCreation :: widgetInit
      }
      ConstructorMethod(widgets map {w => boxCreation ++ ports ++ w } getOrElse (boxCreation ++ ports))
    }
    def appl(b: BoxDef) = {
      val bs = b.symbol.asInstanceOf[BoxTypeSymbol]
      // propagate initial inputs
      def execConnection(c: ConnectionDef) = {
        def toRef(p: Tree): Tree = p match {
          case ThisRef ⇒ This
          case v: ValRef ⇒ Select(This, FieldRef(v.name, v.tpe.asInstanceOf[BoxTypeSymbol].fqName, bs.fqName))
          case p@PortRef(ref, _, _) ⇒
            Select(toRef(ref), FieldRef(p.name, p.tpe.name, p.fromRef.tpe.asInstanceOf[BoxTypeSymbol].fqName))
        }
        Assign(toRef(c.b), toRef(c.a))
      }
      def connections = b.connections collect { case c: ConnectionDef ⇒ c }
      def propagateInitialInputs = {
        def initialConnections = connections collect {
          case c@ConnectionDef(p@PortRef(ThisRef, portName, in), b) ⇒ c
        }
        initialConnections map { execConnection(_) }
      }
      // execute in order
      def runOne(v: ValDef) = {
        def outConnections = connections collect {
          case c@ConnectionDef(p@PortRef(vref@ValRef(_), _, _), b) if (vref.symbol == v.symbol) ⇒ c
        }
        val outs = outConnections map { execConnection(_) }
        val tpe = v.tpe.asInstanceOf[BoxTypeSymbol].fqName
        val invoke = Invoke(
          Select(This, FieldRef(v.name, tpe, bs.fqName)),
          "apply",
          List(),
          tpe,
          "()V")
        invoke :: outs
      }

      Method(Name("contents"), "()V",propagateInitialInputs ++ (b.vals flatMap { case v: ValDef ⇒ runOne(v) }))

    }
  }
  def run() = {
    val owner = global.root
    val mutated = orderValDefs(
      swapConnections(t, owner))
    PrettyPrinter.print(mutated, 0)
    rewrite(mutated)
  }
}