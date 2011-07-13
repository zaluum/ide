package org.zaluum.nide.compiler
sealed trait BinOp

sealed abstract class ExprType(nameStr:String) extends BoxType {
  val owner = null
  val name = Name(nameStr)
  val fqName = Name("org.zaluum.math." + nameStr)
  val a = new PortSymbol(this, Name("a"), Point(0, 0), In)
  val b = new PortSymbol(this, Name("b"), Point(0, 0), In)
  val c = new PortSymbol(this, Name("c"), Point(0, 0), Out)
  val ports = List(a, b, c) map { a => (a.name -> a) } toMap
  def lookupPort(a: Name) = ports.get(a)
  def portInstancesOf(v: ValSymbol) = {
    def findPort(p: PortSymbol) = v.portInstances.asInstanceOf[List[RealPortInstance]].find(_.portSymbol == p).get
    (findPort(a), findPort(b), findPort(c))
  }
}
object AddExprType extends ExprType("Add")
object SubExprType extends ExprType("Sub")
object MulExprType extends ExprType("Mul")
object DivExprType extends ExprType("Div")
object RemExprType extends ExprType("Rem")
object Expressions {
	val all = List(AddExprType,SubExprType,MulExprType,DivExprType,RemExprType)
	def find (name:Name) = all find (_.fqName == name)

}