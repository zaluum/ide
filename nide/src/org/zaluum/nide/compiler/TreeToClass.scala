package org.zaluum.nide.compiler
case class BoxClass(name: Name, fields: List[FieldDef], cons: ConstructorMethod, apply: Method) extends Tree
case class FieldDef(name: Name, typeName: Name) extends Tree
case class New(typeName: Name, param : Tree, signature:String) extends Tree
case class ConstructorMethod(boxCreation: List[Tree]) extends Tree
case class Method(name: Name, stats: List[Tree]) extends Tree
case class Assign(lhs: Tree, rhs: Tree) extends Tree
case class Select(a: Tree, b: Tree) extends Tree
case object This extends Tree
case object Pop extends Tree
case object NullConst extends Tree
case class FieldRef(id: Name, typeName: Name, fromClass: Name) extends Tree
case class Invoke(obj: Tree, meth: String, param: List[Tree], fromClass: Name, descriptor: String) extends Tree
case class Const(i:Int) extends Tree
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
      case b@BoxDef(name, image, defs, vals, ports, connections) ⇒
        val orderVals = b.symbol.asInstanceOf[BoxTypeSymbol].executionOrder.reverse map { _.decl }
        println("execution order = " + orderVals)
        atOwner(b.symbol) {
          BoxDef(name, image,
            transformTrees(defs),
            transformTrees(orderVals),
            transformTrees(ports),
            transformTrees(connections))
        }
    }
  }
  object rewrite {
    def apply(t: Tree) = t match {
      case b@BoxDef(name, image, defs, vals, ports, connections) ⇒
        val tpe = b.symbol.asInstanceOf[BoxTypeSymbol]
        val widgetField = FieldDef(Name("_widget"), Name("javax.swing.JComponent"))
        val fields = widgetField :: (vals ++ ports).map { field(_) }
        BoxClass(
          tpe.fqName,
          fields,
          cons(b),
          appl(b))
    }
    def field(t: Tree) = t match {
      case PortDef(name, typeName, dir, inPos, extPos) ⇒
        FieldDef(name, t.symbol.tpe.name)
      case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
        val tpe = v.symbol.tpe.asInstanceOf[BoxTypeSymbol]
        FieldDef(name, t.symbol.tpe.asInstanceOf[BoxTypeSymbol].fqName)
    }
    def cons(b: BoxDef) = {
      val bs = b.symbol.asInstanceOf[BoxTypeSymbol]
      val boxCreation: List[Tree] = b.vals map {
        _ match {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
            val tpe = v.symbol.tpe.asInstanceOf[BoxTypeSymbol]
            Assign(Select(This, FieldRef(name, tpe.fqName, bs.fqName)), New(tpe.fqName,EmptyTree,"()V"))
        }
      }
      val widgetName = Name("_widget")
      val widgetTpe = Name("javax.swing.JComponent")
      val widgetCreation: Tree = 
        Assign(Select(This, FieldRef(widgetName, widgetTpe, bs.fqName)), 
            New(Name("javax.swing.JPanel"), NullConst, "(Ljava/awt/LayoutManager;)V"))
      val widgetInit = b.vals flatMap {
        case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
          val tpe = v.tpe.asInstanceOf[BoxTypeSymbol];
          tpe.visualClass map { cl ⇒
            List[Tree](
               Invoke(
                   Select(Select(This, FieldRef(v.name,tpe.fqName,bs.fqName)),
                       FieldRef(widgetName,widgetTpe,tpe.fqName)),
                   "setBounds", 
                   List(Const(guiPos.map(_.x).getOrElse(0)),
                       Const( guiPos.map(_.y).getOrElse(0)),
                       Const(guiSize.map(_.w).getOrElse(50)),
                       Const(guiSize.map(_.h).getOrElse(50))),
                   Name("javax.swing.JComponent"),
                   "(IIII)V"
                   ),
              Invoke(
                Select(This, FieldRef(widgetName, widgetTpe, bs.fqName)),
                "add",
                List(Select(Select(This, FieldRef(v.name,tpe.fqName,bs.fqName)),
                       FieldRef(widgetName,widgetTpe,tpe.fqName))),
                widgetTpe, "(Ljava/awt/Component;)Ljava/awt/Component;"),
              Pop)
          } getOrElse (List())
      }
      ConstructorMethod(boxCreation ++ (widgetCreation :: widgetInit))
    }
    def appl(b: BoxDef) = {
      val bs = b.symbol.asInstanceOf[BoxTypeSymbol]
      // propagate initial inputs
      def execConnection(c: ConnectionDef) = {
        def toRef(p: Tree): Tree = p match {
          case ThisRef ⇒ This
          case v: ValRef ⇒ Select(This, FieldRef(v.name, v.tpe.asInstanceOf[BoxTypeSymbol].fqName, b.name))
          case p@PortRef(ref, _, _) ⇒
            Select(toRef(ref), FieldRef(p.name, p.tpe.name, p.fromRef.tpe.name))
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

      Method(Name("apply"), propagateInitialInputs ++ b.vals flatMap { case v: ValDef ⇒ runOne(v) })

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