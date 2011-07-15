package org.zaluum.nide.compiler
sealed trait BinOp

sealed abstract class ExprType(nameStr: String) extends BoxType {
  val owner = null
  val name = Name(nameStr)
  val fqName = Name("org.zaluum.expr." + nameStr)
  val o = new PortSymbol(this, Name("o"), Point(0, 0), Out)
  val ports = Map(o.name -> o)
  val params = Map[Name, ParamSymbol]()
  def lookupPort(a: Name) = ports.get(a)
  def lookupParam(a: Name) = params.get(a)
  def outPort(v: ValSymbol) = v.findPortInstance(o).get
}
sealed abstract class UnaryExprType(nameStr: String) extends ExprType(nameStr) {
  val a = new PortSymbol(this, Name("a"), Point(0, 0), In)
  override val ports = Map(a.name -> a, o.name -> o)

  def unaryPortInstancesOf(v: ValSymbol) =
    (v.findPortInstance(a).get, v.findPortInstance(o).get)

}
sealed abstract class BinExprType(nameStr: String) extends ExprType(nameStr) {
  val a = new PortSymbol(this, Name("a"), Point(0, 0), In)
  val b = new PortSymbol(this, Name("b"), Point(0, 0), In)
  override val ports = List(a, b, o) map { a => (a.name -> a) } toMap
  def binaryPortInstancesOf(v: ValSymbol) =
    (v.findPortInstance(a).get, v.findPortInstance(b).get, v.findPortInstance(o).get)

}
sealed abstract class MathExprType(nameStr: String) extends BinExprType(nameStr)
sealed abstract class CmpExprType(nameStr: String) extends BinExprType(nameStr)
sealed abstract class EqualityExprType(nameStr: String) extends BinExprType(nameStr)
sealed abstract class BitBinExprType(nameStr: String) extends BinExprType(nameStr)
sealed abstract class CastExprType(nameStr: String) extends UnaryExprType(nameStr)

object LiteralExprType extends ExprType("Literal") {
  val paramName = Name("literal")
  val paramSymbol = new ParamSymbol(null, paramName)
  override val params = Map(paramName -> paramSymbol)
}

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

object EqExprType extends EqualityExprType("Eq")
object NeExprType extends EqualityExprType("Ne")

object MinusExprType extends UnaryExprType("Minus")
object NotExprType extends UnaryExprType("Not")

object AndExprType extends BitBinExprType("And")
object OrExprType extends BitBinExprType("Or")
object XorExprType extends BitBinExprType("Xor")

object AddExprType extends MathExprType("Add")
object SubExprType extends MathExprType("Sub")
object MulExprType extends MathExprType("Mul")
object DivExprType extends MathExprType("Div")
object RemExprType extends MathExprType("Rem")
object Expressions {
  val all = List(
    LiteralExprType,
    ToByteType,
    ToShortType,
    ToCharType,
    ToIntType,
    ToLongType,
    ToFloatType,
    ToDoubleType,
    LtExprType,
    LtExprType,
    LeExprType,
    GtExprType,
    GeExprType,
    EqExprType,
    NeExprType,
    OrExprType,
    AndExprType,
    XorExprType,
    MinusExprType,
    NotExprType,
    AddExprType,
    SubExprType,
    MulExprType,
    DivExprType,
    RemExprType) map { e => e.fqName -> e } toMap
  def find(name: Name) = all.get(name)

}