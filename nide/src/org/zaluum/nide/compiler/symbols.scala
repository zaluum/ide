package org.zaluum.nide.compiler

trait Symbol {
  def owner: Symbol
  def name: Name
  var decl: Tree = EmptyTree
  var tpe: Type = NoSymbol
  var scope: Scope = null
}
trait Type extends Symbol
case object NoSymbol extends Symbol with Type {
  val owner = NoSymbol
  val name = null
}
class PrimitiveJavaType(val owner:Symbol, val name:Name) extends Symbol with Type{
  scope=owner.scope
}
class BoxTypeSymbol(val owner: Symbol, val name: Name, val image:Option[String]) extends LocalScope(owner.scope) with Symbol with Type {
  var source : String = "" // TODO
  def valsInOrder = boxes.values.toList.sortWith(_.name.str< _.name.str).asInstanceOf[List[ValSymbol]]
  def portsInOrder = ports.values.toList.sortWith(_.name.str<_.name.str).asInstanceOf[List[PortSymbol]]
  var executionOrder = List[ValSymbol]() 
  def fqName : Name = owner match {
    case bown:BoxTypeSymbol => Name(bown.fqName.str + "$" + name.str)
    case _ => name
  }
}

class ConnectionSymbol(val owner:Symbol, val name:Name, val from:Tree, val to:Tree) extends Symbol 
// TODO make two classes one that has values from the declaring tree and the other directly from symbol 
class PortSymbol(val owner: Symbol, val name: Name, val extPos:Point, val dir:PortDir) extends Symbol {  
  def box = owner.asInstanceOf[BoxTypeSymbol]
  override def toString = "PortSymbol(" + name + ")"
}
class ValSymbol(val owner: Symbol, val name: Name) extends Symbol {
  override def toString = "ValSymbol(" + name + ")"
}
