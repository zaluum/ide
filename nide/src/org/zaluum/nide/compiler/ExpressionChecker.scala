package org.zaluum.nide.compiler

class ExpressionChecker(val c: CheckConnections) extends CheckerPart {
  def checkBinExprTypes(vs: ValSymbol) {
    import primitives._

    val s = vs.tpe.asInstanceOf[BinExprType]
    val (a, b, o) = s.binaryPortInstancesOf(vs)
      def assignAll(tpe: Type, outTpe: Type) = {
        a.finalTpe = tpe
        b.finalTpe = tpe
        o.finalTpe = outTpe
      }

    val at = unboxIfNeeded(fromTpe(a))
    val bt = unboxIfNeeded(fromTpe(b))
    val (one, other) = (at, bt) match {
      case (NoSymbol, NoSymbol) ⇒ (None, None)
      case (NoSymbol, bt)       ⇒ (Some(bt), None)
      case (at, NoSymbol)       ⇒ (Some(at), None)
      case (at, bt)             ⇒ (Some(at), Some(bt))
    }

    s match {
      case b: BitBinExprType ⇒
        (one, other) match {
          case (Some(primitives.Boolean), Some(primitives.Boolean)) ⇒ assignAll(Boolean, Boolean)
          case (Some(primitives.Boolean), None) ⇒ assignAll(Boolean, Boolean)
          case (Some(p), None) if isIntNumeric(p) ⇒ assignAll(Int, Boolean)
          case (Some(p), Some(p2)) if isIntNumeric(p) && isIntNumeric(p2) ⇒ assignAll(Int, Boolean)
          case (None, _) ⇒ assignAll(Int, Boolean)
          case _ ⇒ error("Incompatible types", vs.decl)
        }
      case s: ShiftExprType ⇒
        if (isIntNumeric(bt) || bt == NoSymbol) {
          if (isIntNumeric(at) || at == NoSymbol) {
            assignAll(Int, Int)
          } else if (at == Long) {
            a.finalTpe = Long; b.finalTpe = Int; o.finalTpe = Long
          } else
            error("Shift only operates on Int and Long", blame(a).getOrElse(vs.decl))

        } else error("Shift distance must be of Int type", blame(a).getOrElse(vs.decl))
      case c: CmpExprType ⇒
        (one, other) match {
          case (Some(p1: PrimitiveJavaType), None) if isNumeric(p1) ⇒ assignAll(toOperationType(p1), Boolean)
          case (Some(p1: PrimitiveJavaType), Some(p2: PrimitiveJavaType)) if isNumeric(p1) && isNumeric(p2) ⇒ assignAll(toOperationType(p1), Boolean)
          case (None, _) ⇒ assignAll(Int, Boolean)
          case _ ⇒ error("Incompatible types", vs.decl)
        }
      case e: EqualityExprType ⇒
        (one, other) match {
          case (Some(p1: PrimitiveJavaType), None) if isNumeric(p1) ⇒ assignAll(toOperationType(p1), Boolean)
          case (Some(p1: PrimitiveJavaType), Some(p2)) if isNumeric(p1) && isNumeric(p2) ⇒ assignAll(toOperationType(p1), Boolean)
          case (None, _) ⇒ assignAll(Int, Boolean)
          case (Some(p1), None) if p1 == primitives.Boolean ⇒ assignAll(Boolean, Boolean)
          case (Some(p1), Some(p2)) if p1 == p2 ⇒ assignAll(p1, Boolean)
          case _ ⇒ error("Incompatible types", vs.decl)
        }
      case _ ⇒
        (one, other) match {
          case (Some(p1: PrimitiveJavaType), None) if isNumeric(p1) ⇒ val t = toOperationType(p1); assignAll(t, t)
          case (Some(p1: PrimitiveJavaType), Some(p2: PrimitiveJavaType)) if isNumeric(p1) && isNumeric(p2) ⇒
            val t = largerOperation(toOperationType(p1), toOperationType(p2))
            assignAll(t, t)
          case (None, _) ⇒ assignAll(Int, Int)
          case _         ⇒ error("Incompatible types", vs.decl)
        }
    }
  }
  def checkUnaryExprType(vs: ValSymbol) {
    import primitives._
    val e = vs.tpe.asInstanceOf[UnaryExprType]
    val (a, o) = e.unaryPortInstancesOf(vs)
    e match {
      case e: CastExprType ⇒ checkCastExprTypes(vs)
      case MinusExprType ⇒
        unboxIfNeeded(fromTpe(a)) match {
          case p: PrimitiveJavaType if isNumeric(p) ⇒
            val t = toOperationType(p)
            a.finalTpe = t; o.finalTpe = t
          case NoSymbol ⇒ a.finalTpe = Int; o.finalTpe = Int
          case _        ⇒ error("Incompatible type", blame(a).get)
        }
      case NotExprType ⇒
        unboxIfNeeded(fromTpe(a)) match {
          case Boolean              ⇒ a.finalTpe = Boolean; o.finalTpe = Boolean
          case p if isIntNumeric(p) ⇒ a.finalTpe = Int; o.finalTpe = Int
          case NoSymbol             ⇒ a.finalTpe = Boolean; o.finalTpe = Boolean
          case _                    ⇒ error("Incompatible type", blame(a).get)
        }
    }
  }
  def checkCastExprTypes(vs: ValSymbol) {
    import primitives._
    val e = vs.tpe.asInstanceOf[CastExprType]
    val (a, o) = e.unaryPortInstancesOf(vs)
    e match {
      case ToByteType   ⇒ o.finalTpe = Byte
      case ToShortType  ⇒ o.finalTpe = Short
      case ToCharType   ⇒ o.finalTpe = Char
      case ToIntType    ⇒ o.finalTpe = Int
      case ToLongType   ⇒ o.finalTpe = Long
      case ToFloatType  ⇒ o.finalTpe = Float
      case ToDoubleType ⇒ o.finalTpe = Double
    }
    connectedFrom(a).map { case (pi, blame) ⇒ (unboxIfNeeded(pi.finalTpe), blame) } match {
      case Some((t, blame)) ⇒ t match {
        case j: PrimitiveJavaType if isNumeric(j) ⇒ a.finalTpe = j
        case _                                    ⇒ a.finalTpe = o.finalTpe; error("Cast between incompatible types", blame)
      }
      case None ⇒ a.finalTpe = o.finalTpe
    }
  }
  def checkLiteralExprType(vs: ValSymbol) {
    val l = LiteralExprType
    val o = l.outPort(vs)
    val t = vs.params.headOption match {
      case Some((p, vuntrimmed: String)) ⇒
        p.tpe = cud.zaluumScope.getZJavaLangString
        val v = vuntrimmed.trim
        Literals.parseNarrowestLiteral(v, cud.zaluumScope) match {
          case Some((_, tpe)) ⇒ o.finalTpe = tpe
          case None           ⇒ error("Cannot parse literal " + v, vs.decl)
        }
      case e ⇒
        o.finalTpe = primitives.Byte;
    }
  }

}