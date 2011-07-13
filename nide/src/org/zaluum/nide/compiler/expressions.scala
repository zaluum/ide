package org.zaluum.nide.compiler
sealed trait BinOp

sealed abstract class ExprType(nameStr:String) extends BoxType {
  val owner = null
  val name = Name(nameStr)
  val fqName = Name("org.zaluum.expr." + nameStr)
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
sealed abstract class MathExprType(nameStr:String) extends ExprType(nameStr)
sealed abstract class CmpExprType(nameStr:String) extends ExprType(nameStr)

object LtExprType extends CmpExprType("Lt")
object LeExprType extends CmpExprType("Le")
object GtExprType extends CmpExprType("Gt")
object GeExprType extends CmpExprType("Ge")
object EqExprType extends CmpExprType("Eq")
object NeExprType extends CmpExprType("Ne")

object AddExprType extends MathExprType("Add")
object SubExprType extends MathExprType("Sub")
object MulExprType extends MathExprType("Mul")
object DivExprType extends MathExprType("Div")
object RemExprType extends MathExprType("Rem")
object Expressions {
	val all = List(
	    AddExprType,
	    SubExprType,
	    MulExprType,
	    DivExprType,
	    RemExprType,
	    LtExprType,
	    LtExprType,
		LeExprType,
		GtExprType,
		GeExprType,
		EqExprType,
		NeExprType
	)
	def find (name:Name) = all find (_.fqName == name)

}