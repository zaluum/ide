package org.zaluum.nide.compiler

import org.eclipse.jdt.internal.compiler.lookup.BaseTypeBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding

object primitives {
  private def n(str: String, desc: String, b: BaseTypeBinding, javaType: Class[_], boxedName: Name, boxMethod: String, size: Int = 1) = {
    new PrimitiveJavaType(Name(str), desc, size, boxedName, boxMethod, b, javaType)
  }
  val Byte = n("byte", "B", TypeBinding.BYTE, java.lang.Byte.TYPE, Name("java.lang.Byte"), "byteValue")
  val Short = n("short", "S", TypeBinding.SHORT, java.lang.Short.TYPE, Name("java.lang.Short"), "shortValue")
  val Int = n("int", "I", TypeBinding.INT, java.lang.Integer.TYPE, Name("java.lang.Integer"), "intValue")
  val Long = n("long", "J", TypeBinding.LONG, java.lang.Long.TYPE, Name("java.lang.Long"), "longValue", 2)
  val Float = n("float", "F", TypeBinding.FLOAT, java.lang.Float.TYPE, Name("java.lang.Float"), "floatValue")
  val Double = n("double", "D", TypeBinding.DOUBLE, java.lang.Double.TYPE, Name("java.lang.Double"), "doubleValue", 2)
  val Boolean = n("boolean", "Z", TypeBinding.BOOLEAN, java.lang.Boolean.TYPE, Name("java.lang.Boolean"), "booleanValue")
  val Char = n("char", "C", TypeBinding.CHAR, java.lang.Character.TYPE, Name("java.lang.Character"), "charValue")

  val allTypes = List(Byte, Short, Int, Long, Float, Double, Boolean, Char)
  def numericTypes = List(Byte, Short, Int, Long, Float, Double, Char)
  def widening(from: PrimitiveJavaType, to: PrimitiveJavaType) = {
    from match {
      case Byte  ⇒ List(Short, Int, Long, Float, Double).contains(to)
      case Short ⇒ List(Int, Long, Float, Double).contains(to)
      case Char  ⇒ List(Int, Long, Float, Double).contains(to)
      case Int   ⇒ List(Long, Float, Double).contains(to)
      case Long  ⇒ List(Float, Double).contains(to)
      case Float ⇒ to == Double
      case _     ⇒ false
    }
  }
  def fromChar(c: Char) = {
    allTypes.find(_.descriptor == c.toString).get
  }

  def toOperationType(t: PrimitiveJavaType): PrimitiveJavaType = {
    t match {
      case Byte   ⇒ Int
      case Short  ⇒ Int
      case Char   ⇒ Int
      case Int    ⇒ Int
      case Long   ⇒ Long
      case Float  ⇒ Float
      case Double ⇒ Double
    }
  }
  def getUnboxedType(p: ClassJavaType): Option[PrimitiveJavaType] = {
    p.name.str match {
      case "java.lang.Boolean" ⇒ Some(primitives.Boolean)
      case "java.lang.Char"    ⇒ Some(primitives.Char)
      case "java.lang.Byte"    ⇒ Some(primitives.Byte)
      case "java.lang.Short"   ⇒ Some(primitives.Short)
      case "java.lang.Integer" ⇒ Some(primitives.Int)
      case "java.lang.Float"   ⇒ Some(primitives.Float)
      case "java.lang.Double"  ⇒ Some(primitives.Double)
      case "java.lang.Long"    ⇒ Some(primitives.Long)
      case _                   ⇒ None
    }
  }
  /** must be int long float or double (operationtype) */
  def largerOperation(a: PrimitiveJavaType, b: PrimitiveJavaType): PrimitiveJavaType = {
    val l = List(Int, Long, Float, Double)
    l(math.max(l.indexOf(a), l.indexOf(b)))
  }
  def isNumeric(tpe: JavaType): Boolean = tpe match {
    case p: PrimitiveJavaType if (p != primitives.Boolean) ⇒ true
    case _ ⇒ false
  }
  def isNumeric(tpe: Option[JavaType]): Boolean = tpe match {
    case Some(p) ⇒ isNumeric(p)
    case _       ⇒ false
  }
  def isObject(tpe: Option[JavaType]): Boolean = {
    tpe match {
      case Some(c: ClassJavaType) ⇒ true
      case Some(a: ArrayType)     ⇒ true
      case _                      ⇒ false
    }
  }
  def isIntNumeric(p: JavaType): Boolean =
    p == primitives.Int ||
      p == primitives.Short ||
      p == primitives.Byte ||
      p == primitives.Char

  def find(desc: String) = allTypes.find(_.descriptor == desc)
  def find(name: Name) = allTypes.find(_.name == name)
}