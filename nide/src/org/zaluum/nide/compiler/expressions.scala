package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.ReferenceBinding
import org.zaluum.nide.zge.ParamProperty
import org.zaluum.nide.zge.Controller
import org.zaluum.nide.zge.TypeParamProperty
import org.zaluum.nide.zge.TextParamProperty
import org.zaluum.nide.zge.ConstructorParamProperty
import org.zaluum.nide.zge.MethodParamProperty
import org.zaluum.nide.zge.FieldParamProperty
import org.zaluum.nide.zge.TextListParamProperty
import org.zaluum.nide.zge.MultiLineTextParamProperty

sealed trait ExprType extends Type with PortsSymbol with PropertySourceType {
  def matchingClass: Class[_]
  lazy val name = Name(matchingClass.getSimpleName)
  lazy val fqName = Name(matchingClass.getName)
  var exprParams = Map[Name, ParamDecl]()
  def addParam(p: ParamDecl) { exprParams += (p.name -> p) }
  var props = List[(Controller, ValDef) ⇒ ParamProperty]()
  def properties(controller: Controller, valDef: ValDef) =
    props.map { _(controller, valDef) }
  def lookupExprParam(a: Name) = exprParams.get(a)
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
  def matchingClass = classOf[org.zaluum.control.If]
  val requiredBlocks = 2
  val cond = new PortSymbol(this, Name("cond"), In)
  ports += (cond.name -> cond)
  def condPort(v: ValSymbol) = v.findPortInstance(cond).get
}
object WhileExprType extends TemplateExprType {
  def matchingClass = classOf[org.zaluum.control.While]
  val requiredBlocks = 1
  val end = new PortSymbol(this, Name("cond"), Out)
  ports += (end.name -> end)
  def endPort(v: ValSymbol) = v.findPortInstance(end).get
}
trait SignatureExprType extends ExprType {
  val signatureSymbol = new ParamDecl(Name(signatureName))
  def signatureName = "#Method"
  addParam(signatureSymbol)
  def signatureProp(c: Controller, v: ValDef): ParamProperty
  props ::= ((c: Controller, v: ValDef) ⇒ signatureProp(c, v))
}
sealed abstract class ThisExprType(val matchingClass: Class[_]) extends ExprType {
  val thiz = new PortSymbol(this, Name("objectIn"), In)
  val thizOut = new PortSymbol(this, Name("objectOut"), Out)
  ports += (thiz.name -> thiz)
  ports += (thizOut.name -> thizOut)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
  def thisOutPort(vs: ValSymbol) = vs.findPortInstance(thizOut).get
}
object ThisRefExprType extends ExprType {
  val matchingClass = classOf[org.zaluum.`object`.This]
  val thiz = new PortSymbol(this, Name("this"), Out)
  ports += (thiz.name -> thiz)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
}
trait TypeParamExprType extends ExprType {
  val typeSymbol = new ParamDecl(Name("#Class"))
  addParam(typeSymbol)
  props ::= ((c: Controller, v: ValDef) ⇒ new TypeParamProperty(c, typeSymbol, v))
}
sealed abstract class StaticExprType(val matchingClass: Class[_]) extends TypeParamExprType

object BoxExprType extends StaticExprType(classOf[org.zaluum.`object`.BoxInstance]) with SignatureExprType {
  val fieldsDecl = new ParamDecl(Name("#Fields"))
  val constructorParamsDecl = new ParamDecl(Name("#Constructor values"))
  val constructorTypesDecl = new ParamDecl(Name("#Constructor types"))
  val scriptDecl = new ParamDecl(Name("#Script"))
  addParam(fieldsDecl)
  addParam(constructorParamsDecl)
  addParam(constructorTypesDecl)
  addParam(scriptDecl)
  props ::= ((c: Controller, v: ValDef) ⇒ new TextListParamProperty(c, fieldsDecl, v))
  props ::= ((c: Controller, v: ValDef) ⇒ new MultiLineTextParamProperty(c, scriptDecl, v))

  def signatureProp(c: Controller, v: ValDef) =
    new MethodParamProperty(c, signatureSymbol, v, Some(v.sym.classinfo), false)
  override def properties(controller: Controller, valDef: ValDef) = {
    val l = valDef.sym.classinfo match {
      case c: PropertySourceType ⇒ c.properties(controller, valDef)
      case _                     ⇒ List()
    }
    l ::: super.properties(controller, valDef)
  }
}
object NewArrayExprType extends StaticExprType(classOf[org.zaluum.`object`.NewArray]) {
  val thiz = new PortSymbol(this, Name("array"), Out)
  ports += (thiz.name -> thiz)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
  def dimensions(v: ValDef) = v.params.find(_.key == NewArrayExprType.arrayDimSymbol.name).map(_.valueStr).getOrElse("1")
  val arrayDimSymbol = new ParamDecl(Name("#Array Dimensions"))
  addParam(arrayDimSymbol)
  props ::= ((c: Controller, v: ValDef) ⇒ new TextParamProperty(c, arrayDimSymbol, v))
}
object NewExprType extends StaticExprType(classOf[org.zaluum.`object`.New]) with SignatureExprType {
  override def signatureName = "#Constructor"
  val thiz = new PortSymbol(this, Name("object"), Out)
  ports += (thiz.name -> thiz)
  def thisPort(vs: ValSymbol) = vs.findPortInstance(thiz).get
  def signatureProp(c: Controller, v: ValDef) =
    new ConstructorParamProperty(c, signatureSymbol, v, thisPort(v.sym).tpe)
}
object InvokeExprType extends ThisExprType(classOf[org.zaluum.`object`.Invoke]) with SignatureExprType {
  def signatureProp(c: Controller, v: ValDef) =
    new MethodParamProperty(c, signatureSymbol, v, thisPort(v.sym).tpe, false)
}
object InvokeStaticExprType extends StaticExprType(classOf[org.zaluum.`object`.InvokeStatic]) with SignatureExprType {
  def signatureProp(c: Controller, v: ValDef) =
    new MethodParamProperty(c, signatureSymbol, v, Some(v.sym.classinfo), true)
}
object FieldExprType extends ThisExprType(classOf[org.zaluum.`object`.Field]) with ResultExprType with OneParameter with SignatureExprType {
  override def signatureName = "#Field"
  def signatureProp(c: Controller, v: ValDef) =
    new FieldParamProperty(c, signatureSymbol, v, thisPort(v.sym).tpe, false)
}
object StaticFieldExprType extends StaticExprType(classOf[org.zaluum.`object`.FieldStatic]) with ResultExprType with OneParameter with SignatureExprType {
  override def signatureName = "#Field"
  def signatureProp(c: Controller, v: ValDef) =
    new FieldParamProperty(c, signatureSymbol, v, Some(v.sym.classinfo), true)
}
object ArrayExprType extends ThisExprType(classOf[org.zaluum.`object`.Array]) with ResultExprType with OneParameter {
  val index = new PortSymbol(this, Name("index"), In)
  ports += (index.name -> index)
  def indexPort(vs: ValSymbol) = vs.findPortInstance(index).get
}
object LiteralExprType extends ResultExprType {
  def matchingClass = classOf[org.zaluum.op.Literal]
  val paramDecl = new ParamDecl(Name("literal"))
  addParam(paramDecl)
  props ::= ((c: Controller, v: ValDef) ⇒ new TextParamProperty(c, paramDecl, v))
}

object CastToExprType extends CastExprType(classOf[org.zaluum.op.Cast]) with TypeParamExprType

object ShiftLeftExprType extends ShiftExprType(classOf[org.zaluum.op.ShiftLeft])
object UShiftRightExprType extends ShiftExprType(classOf[org.zaluum.op.UShiftRight])
object ShiftRightExprType extends ShiftExprType(classOf[org.zaluum.op.ShiftRight])

object LtExprType extends CmpExprType(classOf[org.zaluum.op.Lt])
object LeExprType extends CmpExprType(classOf[org.zaluum.op.Le])
object GtExprType extends CmpExprType(classOf[org.zaluum.op.Gt])
object GeExprType extends CmpExprType(classOf[org.zaluum.op.Ge])

object EqExprType extends EqualityExprType(classOf[org.zaluum.op.Eq])
object NeExprType extends EqualityExprType(classOf[org.zaluum.op.Ne])

object MinusExprType extends UnaryExprType(classOf[org.zaluum.op.Minus])
object NotExprType extends UnaryExprType(classOf[org.zaluum.op.Not])

object AndExprType extends BitBinExprType(classOf[org.zaluum.op.And])
object OrExprType extends BitBinExprType(classOf[org.zaluum.op.Or])
object XorExprType extends BitBinExprType(classOf[org.zaluum.op.Xor])

object AddExprType extends MathExprType(classOf[org.zaluum.op.Add])
object SubExprType extends MathExprType(classOf[org.zaluum.op.Sub])
object MulExprType extends MathExprType(classOf[org.zaluum.op.Mul])
object DivExprType extends MathExprType(classOf[org.zaluum.op.Div])
object RemExprType extends MathExprType(classOf[org.zaluum.op.Rem])
object Expressions {
  val all = List(
    BoxExprType,
    ThisRefExprType,
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
    ShiftLeftExprType,
    UShiftRightExprType,
    ShiftRightExprType,
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
      f != ArrayExprType && f != BoxExprType &&
        (f.isInstanceOf[ThisExprType] || f.isInstanceOf[StaticExprType])
    }.map { e ⇒ e.fqName -> e } toMap
  val templateExpressions = List(
    IfExprType,
    WhileExprType) map { e ⇒ e.fqName -> e } toMap
  def find(name: Name) = all.get(name)
  def isTemplateExpression(className: Name) = templateExpressions.contains(className)

}