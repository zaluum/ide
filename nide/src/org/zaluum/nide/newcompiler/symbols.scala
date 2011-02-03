package org.zaluum.nide.newcompiler

import org.zaluum.nide.model.Point
import org.zaluum.nide.model.Positionable
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
class BoxTypeSymbol(val owner: Symbol, val name: Name) extends LocalScope(Some(owner.scope)) with Symbol with Type 

 
// TODO make two classes one that has values from the declaring tree and the other directly from symbol 
class PortSymbol(val owner: Symbol, val name: Name, val extPos:Point, val in:Boolean) extends Symbol {  
  def box = owner.asInstanceOf[BoxTypeSymbol]
}
class ValSymbol(val owner: Symbol, val name: Name) extends Symbol
