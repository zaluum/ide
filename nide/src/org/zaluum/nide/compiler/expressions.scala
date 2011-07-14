package org.zaluum.nide.compiler
sealed trait BinOp

sealed abstract class ExprType(nameStr: String) extends BoxType {
  val owner = null
  val name = Name(nameStr)
  val fqName = Name("org.zaluum.expr." + nameStr)
  val a = new PortSymbol(this, Name("a"), Point(0, 0), In)
  val o = new PortSymbol(this, Name("o"), Point(0, 0), Out)
  val ports = List(a, o) map { a => (a.name -> a) } toMap
  def lookupPort(a: Name) = ports.get(a)
  def unaryPortInstancesOf(v: ValSymbol) = 
    (v.findPortInstance(a).get, v.findPortInstance(o).get)
}

sealed abstract class BinExprType(nameStr:String) extends ExprType(nameStr) {
  val b = new PortSymbol(this, Name("b"), Point(0, 0), In)
  override val ports = List(a, b, o) map { a => (a.name -> a) } toMap
  def binaryPortInstancesOf(v: ValSymbol) = 
    (v.findPortInstance(a).get, v.findPortInstance(b).get,v.findPortInstance(o).get)
  
}
sealed abstract class MathExprType(nameStr: String) extends BinExprType(nameStr)
sealed abstract class CmpExprType(nameStr: String) extends BinExprType(nameStr)
sealed abstract class CastExprType(nameStr: String) extends ExprType(nameStr)

object ToByteType extends CastExprType("ToByte")
object ToShortType extends CastExprType("ToShort")
object ToCharType extends CastExprType("ToChar")
object ToIntType extends CastExprType("ToInt")
object ToLongType extends CastExprType("ToLong")
object ToFloatType extends CastExprType("ToFloat")
object ToDoubleType extends CastExprType("ToDouble")

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
    ToByteType,
    ToShortType,
    ToCharType,
    ToIntType,
    ToLongType,
    ToFloatType,
    ToDoubleType,
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
    NeExprType)
  def find(name: Name) = all find (_.fqName == name)

}