package org.zaluum.nide.compiler

class ExpressionChecker(val c: CheckConnections) extends CheckerPart {
  def checkBinExprTypes(vs: ValSymbol) {
    import primitives._

    val s = vs.tpe.get.asInstanceOf[BinExprType]
    val (a, b, o) = s.binaryPortInstancesOf(vs)
      def assignAll(tpe: JavaType, outTpe: JavaType) = {
        a.tpe = Some(tpe)
        b.tpe = Some(tpe)
        o.tpe = Some(outTpe)
      }

    val at = unboxIfNeeded(fromTpe(a))
    val bt = unboxIfNeeded(fromTpe(b))
    val (one, other) = (at, bt) match {
      case (None, None) ⇒ (None, None)
      case (None, bt)   ⇒ (bt, None)
      case (at, None)   ⇒ (at, None)
      case (at, bt)     ⇒ (at, bt)
    }

    s match {
      case b: BitBinExprType ⇒
        (one, other) match {
          case (Some(primitives.Boolean), Some(primitives.Boolean)) ⇒ assignAll(Boolean, Boolean)
          case (Some(primitives.Boolean), None) ⇒ assignAll(Boolean, Boolean)
          case (Some(p), None) if isIntNumeric(p) ⇒ assignAll(Int, Boolean)
          case (Some(p), Some(p2)) if isIntNumeric(p) && isIntNumeric(p2) ⇒ assignAll(Int, Boolean)
          case (None, _) ⇒ assignAll(Int, Boolean)
          case _ ⇒
            assignAll(Int, Boolean)
            error("Binary operation with incompatible types", vs.decl)
        }
      case s: ShiftExprType ⇒
        if (bt.isEmpty || isIntNumeric(bt.get)) {
          if (at.isEmpty || isIntNumeric(at.get)) {
            assignAll(Int, Int)
          } else if (at == Long) {
            a.tpe = Long; b.tpe = Int; o.tpe = Long
          } else
            error("Shift only operates on Int and Long", blame(a).getOrElse(vs.decl))

        } else error("Shift distance must be of Int type", blame(a).getOrElse(vs.decl))
      case c: CmpExprType ⇒
        (one, other) match {
          case (Some(p1: PrimitiveJavaType), None) if isNumeric(p1) ⇒ assignAll(toOperationType(p1), Boolean)
          case (Some(p1: PrimitiveJavaType), Some(p2: PrimitiveJavaType)) if isNumeric(p1) && isNumeric(p2) ⇒ assignAll(toOperationType(p1), Boolean)
          case (None, _) ⇒ assignAll(Int, Boolean)
          case _ ⇒
            assignAll(Int, Boolean)
            error("Comparation between incompatible types", vs.decl)
        }
      case e: EqualityExprType ⇒
        (one, other) match {
          case (Some(p1: PrimitiveJavaType), None) if isNumeric(p1) ⇒ assignAll(toOperationType(p1), Boolean)
          case (Some(p1: PrimitiveJavaType), Some(p2)) if isNumeric(p1) && isNumeric(p2) ⇒ assignAll(toOperationType(p1), Boolean)
          case (None, _) ⇒ assignAll(Int, Boolean)
          case (Some(p1), None) if p1 == primitives.Boolean ⇒ assignAll(Boolean, Boolean)
          case (Some(p1), Some(p2)) if p1 == p2 ⇒ assignAll(p1, Boolean)
          case _ ⇒
            assignAll(Int, Boolean)
            error("Relation test between incompatible types", vs.decl)
        }
      case _ ⇒
        (one, other) match {
          case (Some(p1: PrimitiveJavaType), None) if isNumeric(p1) ⇒ val t = toOperationType(p1); assignAll(t, t)
          case (Some(p1: PrimitiveJavaType), Some(p2: PrimitiveJavaType)) if isNumeric(p1) && isNumeric(p2) ⇒
            val t = largerOperation(toOperationType(p1), toOperationType(p2))
            assignAll(t, t)
          case (None, _) ⇒ assignAll(Int, Int)
          case _         ⇒ assignAll(Int, Int); error("Operation between incompatible types", vs.decl)
        }
    }
  }
  def checkUnaryExprType(vs: ValSymbol) {
    import primitives._
    val e = vs.tpe.get.asInstanceOf[UnaryExprType]
    val (a, o) = e.unaryPortInstancesOf(vs)
    e match {
      case e: CastExprType ⇒ checkCastExprTypes(vs)
      case MinusExprType ⇒
        unboxIfNeeded(fromTpe(a)) match {
          case Some(p: PrimitiveJavaType) if isNumeric(p) ⇒
            val t = toOperationType(p)
            a.tpe = t; o.tpe = t
          case None ⇒ a.tpe = Int; o.tpe = Int
          case _    ⇒ error("Cannot change sign of non numeric type", blame(a).get)
        }
      case NotExprType ⇒
        unboxIfNeeded(fromTpe(a)) match {
          case Some(Boolean)              ⇒ a.tpe = Boolean; o.tpe = Boolean
          case Some(p) if isIntNumeric(p) ⇒ a.tpe = Int; o.tpe = Int
          case None                       ⇒ a.tpe = Boolean; o.tpe = Boolean
          case Some(t)                    ⇒ error("Cannot apply NOT to type " + t.fqName.str, blame(a).get)
        }
    }
  }
  def checkCastExprTypes(vs: ValSymbol) {
    import primitives._
    val e = vs.tpe.get.asInstanceOf[CastExprType]
    val (a, o) = e.unaryPortInstancesOf(vs)
    e match {
      case ToByteType   ⇒ o.tpe = Byte
      case ToShortType  ⇒ o.tpe = Short
      case ToCharType   ⇒ o.tpe = Char
      case ToIntType    ⇒ o.tpe = Int
      case ToLongType   ⇒ o.tpe = Long
      case ToFloatType  ⇒ o.tpe = Float
      case ToDoubleType ⇒ o.tpe = Double
      case CastToExprType ⇒
        vs.params.get(CastToExprType.typeSymbol) match {
          case Some(v) ⇒
            o.tpe = ztd.zaluumScope.lookupType(Name(v.encoded)) orElse {
              error("Cast type " + v.encoded + " not found", vs.decl)
              None
            }
          case _ ⇒
            o.tpe = None
            error("Cast type not specified", vs.decl)
        }
    }
    connectedFrom(a).map {
      case (pi, blame) ⇒ (pi.tpe, blame)
    } match {
      case Some((fromType, blame)) ⇒
        a.tpe = fromType
        (fromType, o.tpe) match {
          case (Some(from), Some(to)) ⇒ (from, to) match {
            // TODO serializable and other specs exceptions
            case (fromPrim: PrimitiveJavaType, toPrim: PrimitiveJavaType) if (isNumeric(fromPrim) && isNumeric(toPrim)) ⇒
            case (fromPrim: PrimitiveJavaType, toClass: ClassJavaType) if (primitives.getUnboxedType(toClass) == Some(fromPrim)) ⇒
            case (fromClass: ClassJavaType, toPrim: PrimitiveJavaType) if (primitives.getUnboxedType(fromClass) == Some(toPrim)) ⇒
            case (fromClass: ClassJavaType, toClass: ClassJavaType) if (fromClass.binding.isCompatibleWith(toClass.binding)) ⇒
            case (fromClass: ClassJavaType, toArray: ArrayType) if (fromClass.fqName.str == "java.lang.Object") ⇒
            case (fromArray: ArrayType, toClass: ClassJavaType) if (toClass.fqName.str == "java.lang.Object") ⇒
            case (fromArray: ArrayType, toArray: ArrayType) if (fromArray == toArray) ⇒
          }
          case _ ⇒
            a.tpe = None
            error("Cast between incompatible types", blame)
        }
      case None ⇒ a.tpe = o.tpe
    }
  }
  def checkLiteralExprType(vs: ValSymbol) {
    val l = LiteralExprType
    val o = l.outPort(vs)
    assert(vs.tdecl.params.size <= 1)
    val t = vs.tdecl.params.headOption match {
      case Some(param: Param) ⇒
        val value = Values.parseNarrowestLiteral(param.value, ztd.zaluumScope) /* TODO pass project */
        o.tpe = ztd.zaluumScope.lookupType(value.valueTpe.tpe)
        vs.params = Map(LiteralExprType.paramSymbol -> value)
        if (!value.valid || o.tpe.isEmpty)
          error("Cannot parse literal " + param.value, vs.decl)
      case e ⇒
        o.tpe = primitives.Byte;
    }
  }

}