package org.zaluum.nide.compiler
sealed trait BinOp

sealed trait ExprType extends BoxType {
  val owner = null
  def matchingClass: Class[_]
  lazy val name = Name(matchingClass.getSimpleName)
  lazy val fqName = Name(matchingClass.getName)
  var params = Map[Name, ParamSymbol]()
  def lookupParam(a: Name) = params.get(a)
  def templateTree = null
}
sealed trait ResultExprType extends ExprType {
  val o = new PortSymbol(this, Name("o"), Out)
  ports += (o.name -> o)
  def outPort(v: ValSymbol) = v.findPortInstance(o).get
}
sealed trait OneParameter extends ExprType {
  val a = new PortSymbol(this, Name("a"), In)
  ports += (a.name -> a)
  def aPort(vs: ValSymbol) = vs.findPortInstance(a).get
}
sealed abstract class UnaryExprType(val matchingClass: Class[_]) extends ResultExprType with OneParameter {
  def unaryPortInstancesOf(v: ValSymbol) =
    (v.findPortInstance(a).get, v.findPortInstance(o).get)
}
sealed abstract class BinExprType(val matchingClass: Class[_]) extends ResultExprType {
  val a = new PortSymbol(this, Name("a"), In)
  val b = new PortSymbol(this, Name("b"), In)
  ports += (a.name -> a)
  ports += (b.name -> b)
  def binaryPortInstancesOf(v: ValSymbol) =
    (v.findPortInstance(a).get, v.findPortInstance(b).get, v.findPortInstance(o).get)

}
sealed abstract class MathExprType(cl: Class[_]) extends BinExprType(cl)
sealed abstract class CmpExprType(cl: Class[_]) extends BinExprType(cl)
sealed abstract class ShiftExprType(cl: Class[_]) extends BinExprType(cl)
sealed abstract class EqualityExprType(cl: Class[_]) extends BinExprType(cl)
sealed abstract class BitBinExprType(cl: Class[_]) extends BinExprType(cl)
sealed abstract class CastExprType(cl: Class[_]) extends UnaryExprType(cl)
sealed abstract class TemplateExprType extends ExprType {
  val requiredBlocks: Int
}
object IfExprType extends TemplateExprType {
  def matchingClass = classOf[org.zaluum.expr.If]
  val requiredBlocks = 2
  val cond = new PortSymbol(this, Name("cond"), In)
  ports += (cond.name -> cond)
  def condPort(v: ValSymbol) = v.findPortInstance(cond).get
}
object WhileExprType extends TemplateExprType {
  def matchingClass = classOf[org.zaluum.expr.While]
  val requiredBlocks = 1
  val end = new PortSymbol(this, Name("cond"), Out)
  ports += (end.name -> end)
  def endPort(v: ValSymbol) = v.findPortInstance(end).get
}
trait SignatureExprType extends ExprType {
  val Sig = """(.+)(\(.*)""".r
  val signatureName = Name("signature")
  val signatureSymbol = new ParamSymbol(null, signatureName)
  params += (signatureName -> signatureSymbol)
}
sealed abstract class ThisExprType(val matchingClass: Class[_]) extends SignatureExprType {
  val thiz = new PortSymbol(this, Name("objectIn"), In)
  val thizOut = new PortSymbol(this, Name("objectOut"), Out)
  ports += (thiz.name -> thiz)
  ports += (thizOut.name -> thizOut)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
  def thisOutPort(vs: ValSymbol) = vs.findPortInstance(thizOut).get

}
trait TypeParamExprType extends ExprType {
  val typeName = Name("type")
  val typeSymbol = new ParamSymbol(null, typeName)
  params += (typeName -> typeSymbol)
}
sealed abstract class StaticExprType(val matchingClass: Class[_]) extends SignatureExprType with TypeParamExprType
object NewArrayExprType extends StaticExprType(classOf[org.zaluum.expr.`object`.NewArray]) {
  val thiz = new PortSymbol(this, Name("array"), Out)
  ports += (thiz.name -> thiz)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
  def dimensions(v: ValDef) = v.params.asInstanceOf[List[Param]].find(_.key == NewArrayExprType.arrayDimName).map(_.value).getOrElse("1")
  val arrayDimName = Name("arrayDim")
  val arrayDimSymbol = new ParamSymbol(null, arrayDimName)
  params += (arrayDimName -> arrayDimSymbol)
}
object NewExprType extends StaticExprType(classOf[org.zaluum.expr.`object`.New]) {
  val thiz = new PortSymbol(this, Name("object"), Out)
  ports += (thiz.name -> thiz)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
}
object InvokeExprType extends ThisExprType(classOf[org.zaluum.expr.`object`.Invoke])
object InvokeStaticExprType extends StaticExprType(classOf[org.zaluum.expr.`object`.InvokeStatic])
object FieldExprType extends ThisExprType(classOf[org.zaluum.expr.`object`.Field]) with ResultExprType with OneParameter
object StaticFieldExprType extends StaticExprType(classOf[org.zaluum.expr.`object`.StaticField]) with ResultExprType with OneParameter
object ArrayExprType extends ThisExprType(classOf[org.zaluum.expr.`object`.Array]) with ResultExprType with OneParameter {
  val index = new PortSymbol(this, Name("index"), In)
  ports += (index.name -> index)
  def indexPort(vs: ValSymbol) = vs.findPortInstance(index).get
}
object LiteralExprType extends ResultExprType {
  def matchingClass = classOf[org.zaluum.expr.Literal]
  val paramName = Name("literal")
  val paramSymbol = new ParamSymbol(null, paramName)
  params += (paramName -> paramSymbol)
}

object ToByteType extends CastExprType(classOf[org.zaluum.expr.cast.ToByte])
object ToShortType extends CastExprType(classOf[org.zaluum.expr.cast.ToShort])
object ToCharType extends CastExprType(classOf[org.zaluum.expr.cast.ToChar])
object ToIntType extends CastExprType(classOf[org.zaluum.expr.cast.ToInt])
object ToLongType extends CastExprType(classOf[org.zaluum.expr.cast.ToLong])
object ToFloatType extends CastExprType(classOf[org.zaluum.expr.cast.ToFloat])
object ToDoubleType extends CastExprType(classOf[org.zaluum.expr.cast.ToDouble])
object CastToExprType extends CastExprType(classOf[org.zaluum.expr.cast.Cast]) with TypeParamExprType

object ShiftLeftExprType extends ShiftExprType(classOf[org.zaluum.expr.arithmetic.ShiftLeft])
object UShiftRightExprType extends ShiftExprType(classOf[org.zaluum.expr.arithmetic.UShiftRight])
object ShiftRightExprType extends ShiftExprType(classOf[org.zaluum.expr.arithmetic.ShiftRight])

object LtExprType extends CmpExprType(classOf[org.zaluum.expr.compare.Lt])
object LeExprType extends CmpExprType(classOf[org.zaluum.expr.compare.Le])
object GtExprType extends CmpExprType(classOf[org.zaluum.expr.compare.Gt])
object GeExprType extends CmpExprType(classOf[org.zaluum.expr.compare.Ge])

object EqExprType extends EqualityExprType(classOf[org.zaluum.expr.compare.Eq])
object NeExprType extends EqualityExprType(classOf[org.zaluum.expr.compare.Ne])

object MinusExprType extends UnaryExprType(classOf[org.zaluum.expr.arithmetic.Minus])
object NotExprType extends UnaryExprType(classOf[org.zaluum.expr.bool.Not])

object AndExprType extends BitBinExprType(classOf[org.zaluum.expr.bool.And])
object OrExprType extends BitBinExprType(classOf[org.zaluum.expr.bool.Or])
object XorExprType extends BitBinExprType(classOf[org.zaluum.expr.bool.Xor])

object AddExprType extends MathExprType(classOf[org.zaluum.expr.arithmetic.Add])
object SubExprType extends MathExprType(classOf[org.zaluum.expr.arithmetic.Sub])
object MulExprType extends MathExprType(classOf[org.zaluum.expr.arithmetic.Mul])
object DivExprType extends MathExprType(classOf[org.zaluum.expr.arithmetic.Div])
object RemExprType extends MathExprType(classOf[org.zaluum.expr.arithmetic.Rem])
object Expressions {
  val all = List(
    CastToExprType,
    ArrayExprType,
    NewArrayExprType,
    NewExprType,
    InvokeExprType,
    InvokeStaticExprType,
    FieldExprType,
    StaticFieldExprType,
    WhileExprType,
    IfExprType,
    LiteralExprType,
    ToByteType,
    ToShortType,
    ToCharType,
    ToIntType,
    ToLongType,
    ToFloatType,
    ToDoubleType,
    ShiftLeftExprType,
    UShiftRightExprType,
    ShiftRightExprType,
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
    RemExprType) map { e ⇒ e.fqName -> e } toMap
  lazy val thisFigureExpressions =
    all.values.filter { f ⇒
      f != ArrayExprType &&
        (f.isInstanceOf[ThisExprType] || f.isInstanceOf[StaticExprType])
    }.map { e ⇒ e.fqName -> e } toMap
  val templateExpressions = List(
    IfExprType,
    WhileExprType) map { e ⇒ e.fqName -> e } toMap
  def find(name: Name) = all.get(name)
  def isTemplateExpression(className: Name) = templateExpressions.contains(className)

}