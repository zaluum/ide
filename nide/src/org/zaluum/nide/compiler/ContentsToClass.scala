package org.zaluum.nide.compiler

trait ContentsToClass {
  self: TreeToClass =>

  def appl(b: BoxDef): Method = {
    val bs = b.sym
    val connections = bs.connections
    // create locals for expressions
    val localsMap = createLocalsMap(bs)
    //helper methods
    def execConnection(c: (RealPortInstance, Set[RealPortInstance])) = {
      val (out, ins) = c
      ins.toList map { in ⇒
        assign(in, out)
      }
    }
    def assign(to: RealPortInstance, from: RealPortInstance): Assign = {
      Assign(toRef(to), cast(from.finalTpe, to.finalTpe, toRef(from)))
    }
    def toRef(pi: RealPortInstance): Ref = {
      if (pi.valSymbol == bs.thisVal) {
        Select(
          This,
          FieldRef(pi.name, pi.finalTpe.name, bs.fqName))
      } else {
        val vfrom = pi.valSymbol
        vfrom.tpe match {
          case vfromBs: BoxTypeSymbol =>
            Select(
              Select(This, FieldRef(vfrom.name, vfromBs.fqName, bs.fqName)),
              FieldRef(pi.name, pi.finalTpe.name, vfromBs.fqName))
          case _ =>
            LocalRef(localsMap(pi), pi.finalTpe.name)
        }
      }
    }
    def runOne(vs: ValSymbol) = {
      def outConnections = connections.flow collect {
        case c @ (pi, _) if (pi.valSymbol == vs) ⇒ c
      } toList
      val outs = outConnections flatMap { execConnection(_) }
      val invoke = vs.tpe match {
        case vbs: BoxTypeSymbol =>
          val tpe = vbs.fqName
          Invoke(
            Select(This, FieldRef(vs.name, tpe, bs.fqName)),
            "apply",
            List(),
            tpe,
            "()V")
        case s: ExprType =>
          val (a, b, c) = s.portInstancesOf(vs)
          import primitives._
          val aTree = toRef(a)
          val bTree = toRef(b)
          val etpe = a.finalTpe.asInstanceOf[PrimitiveJavaType] // is it safe to pick a?
          val eTree = s match {
            case AddExprType => Add(aTree,bTree,etpe)
            case SubExprType => Sub(aTree,bTree,etpe)
            case MulExprType => Mul(aTree,bTree,etpe)
            case DivExprType => Div(aTree,bTree,etpe)
            case RemExprType => Rem(aTree,bTree,etpe)
            case LtExprType => Lt(aTree,bTree,etpe)
          }
          Assign(toRef(c), eTree)
      }
      invoke :: outs
    }
    // start
    // propagate initial inputs
    val propagateInitialInputs = {
      connections.flow collect {
        case c @ (a, _) if (a.valSymbol == bs.thisVal) ⇒ c
      } flatMap { execConnection(_) }
    }
    // execute in order
    val invokes = bs.valsInOrder flatMap { runOne }
    val localsDecl = localsMap map { case (a, i) => (a.name.str, a.finalTpe.asInstanceOf[JavaType].descriptor, i) } toList;
    Method(Name("contents"), "()V", propagateInitialInputs.toList ++ invokes, localsDecl)
  }

  def cast(from: Type, to: Type, t: Tree): Tree = {
    if (to == from) t
    else {
      (from, to) match {
        case (pf: PrimitiveJavaType, pt: PrimitiveJavaType) =>
          primitiveCast(pf, pt, t)
      }
    }
  }

  def createLocalsMap(bs: BoxTypeSymbol) = {
    var locals = 1; // 0 for "this"
    bs.valsInOrder flatMap { v =>
      v.tpe match {
        case b: BoxTypeSymbol => List()
        case e: ExprType =>
          v.portInstances map { pi =>
            val rpi = pi.asInstanceOf[RealPortInstance]
            (rpi -> { val l = locals; locals = locals + rpi.finalTpe.javaSize; l })
          }
        case _ => List()
      }
    } toMap;
  }
  def primitiveCast(from: PrimitiveJavaType, to: PrimitiveJavaType, t: Tree) = {
    import primitives._
    (from, to) match {
      case (Float, Char) => I2C(F2I(t))
      case (Double, Char) => I2C(D2I(t))
      case (Int, Char) => I2C(t)
      case (Short, Char) => I2C(t)
      case (Byte, Char) => I2C(t)
      case (Long, Char) => I2C(L2I(t))

      case (Char, Float) => I2F(t)
      case (Short, Float) => I2F(t)
      case (Int, Float) => I2F(t)
      case (Byte, Float) => I2F(t)
      case (Double, Float) => D2F(t)
      case (Long, Float) => L2F(t)

      case (Float, Byte) => I2B(F2I(t))
      case (Double, Byte) => I2B(D2I(t))
      case (Int, Byte) => I2B(t)
      case (Short, Byte) => I2B(t)
      case (Char, Byte) => I2B(t)
      case (Long, Byte) => I2B(L2I(t))

      case (Byte, Double) => I2D(t)
      case (Char, Double) => I2D(t)
      case (Short, Double) => I2D(t)
      case (Int, Double) => I2D(t)
      case (Float, Double) => F2D(t)
      case (Long, Double) => L2D(t)

      case (Byte, Short) => I2S(t)
      case (Char, Short) => I2S(t)
      case (Int, Short) => I2S(t)
      case (Double, Short) => I2S(D2I(t))
      case (Long, Short) => I2S(L2I(t))
      case (Float, Short) => I2S(F2I(t))

      case (Double, Int) => D2I(t)
      case (Float, Int) => F2I(t)
      case (Long, Int) => L2I(t)

      case (Int, Long) => I2L(t)
      case (Char, Long) => I2L(t)
      case (Byte, Long) => I2L(t)
      case (Short, Long) => I2L(t)
      case (Double, Long) => D2L(t)
      case (Float, Long) => F2L(t)
      case _ => t
    }
  }
}