package org.zaluum.nide.compiler
case class BoxClass(name: Name, fields: List[FieldDef], cons: ConstructorMethod, apply: ApplyMethod) extends Tree
case class FieldDef(name: Name, typeName: Name) extends Tree
case class PortInitStat(name: Name) extends Tree
case class BoxCreationStat(name: Name, typeName: Name) extends Tree
case class ConstructorMethod(portInit: List[Tree], boxCreation: List[BoxCreationStat]) extends Tree
case class ApplyMethod(stats: List[Tree]) extends Tree
case class Propagate(from: Tree, to: Tree) extends Tree
case class Select(a: Tree, b: Tree) extends Tree
case class RunBox(id: Name, typeName:Name) extends Tree
case object This extends Tree
case class FieldRef(id:Name, typeName:Name, fromClass:Name) extends Tree

class TreeToClass(t: Tree, global: Scope) extends ConnectionHelper with ReporterAdapter {
  val reporter = new Reporter // TODO fail reporter
  def location(t:Tree) = Location(List(0))
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
        val orderVals = b.symbol.asInstanceOf[BoxTypeSymbol].executionOrder map { _.decl }
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
        BoxClass(name, (vals ++ ports) map { field(_) }, cons(ports,vals), appl(b))
    }
    def field(t: Tree) = t match {
      case PortDef(name, typeName, dir, inPos, extPos) ⇒
        FieldDef(name, t.symbol.tpe.name)
      case ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
        FieldDef(name, t.symbol.tpe.name)
    }
    def cons(ports: List[Tree], vals: List[Tree]) = {
      ConstructorMethod (List(), vals map { 
        _ match {
          case v@ValDef(name, typeName, pos, size, guiPos, guiSize) ⇒
            BoxCreationStat(name, v.symbol.tpe.name)
        }
      }
      )
    }
    def appl(b: BoxDef) = {
      // propagate initial inputs
      def execConnection(c: ConnectionDef) = {
        def toRef(p:Tree) : Tree= p match {
          case ThisRef => This
          case v:ValRef => FieldRef(v.name, v.tpe.name, b.name)
          case p@PortRef(ref,_,_) => 
            Select(toRef(ref),FieldRef(p.name, p.tpe.name, p.fromRef.tpe.name))
        }
        Propagate(toRef(c.a), toRef(c.b))
      }
      def connections = b.connections collect { case c: ConnectionDef ⇒ c }
      def propagateInitialInputs = {
        def initialConnections =  connections collect {
          case c@ConnectionDef(p@PortRef(ThisRef, portName, in), b) ⇒ c
        }
        initialConnections map { execConnection(_) }
      }
      // execute in order
      def runOne(v:ValSymbol) = {
        def outConnections = connections collect { 
          case c@ConnectionDef(p@PortRef(vref@ValRef(_),_,_),b) if (vref.symbol == v) => c 
        }
        val outs = outConnections map { execConnection(_) }
        List(RunBox(v.name, v.tpe.name)) ++ outs  
      }
      def runInOrder = {
        b.symbol.asInstanceOf[BoxTypeSymbol].executionOrder flatMap { runOne(_) }
      }
      ApplyMethod(propagateInitialInputs ++ runInOrder)
    }
  }
  def run() = {
    val owner = global.root
    rewrite(orderValDefs(
        swapConnections(t, owner), owner)
    )
  }
}