package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding

trait ContentsToClass {
  self: TreeToClass =>

  def appl(b: BoxDef): Method = {
    val bs = b.sym
    val block = bs.blocks.head
    val connections = block.connections
    // create locals for expressions
    val localsMap = createLocalsMap(bs)
    //helper methods
    def execConnection(c: (PortInstance, Set[PortInstance])) = {
      val (out, ins) = c
      ins.toList map { in ⇒
        assign(in, out)
      }
    }
    def assign(to: PortInstance, from: PortInstance): Assign = {
      Assign(toRef(to), cast(from.finalTpe, to.finalTpe, toRef(from)))
    }
    def toRef(pi: PortInstance): Ref = {
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
      // propagate inputs
      val ins = for (ps <- vs.portSides; if (ps.flowIn); val pi = ps.pi) yield {
        pi.connectedFrom match {
          case Some(o) => assign(pi, o)
          case None => Assign(toRef(pi), Const(0, pi.finalTpe))
        }
      }
      import primitives._
      val invoke : List[Tree] = vs.tpe match {
        case vbs: BoxTypeSymbol =>
          val tpe = vbs.fqName
          List(
            Invoke(
              Select(This, FieldRef(vs.name, tpe, bs.fqName)),
              "apply",
              List(),
              tpe,
              "()V",
              interface = false))
        case InvokeExprType =>
          val m = vs.info.asInstanceOf[MethodBinding]
          val obj = InvokeExprType.thisPort(vs)
          val thisOut = InvokeExprType.thisOutPort(vs) // XXX optimize and use only 1 var
          val params = vs.portSides filter { ps => ps.inPort && ps.pi != obj } sortBy { _.pi.name.str } map { ps => toRef(ps.pi) }
          val invoke = Invoke(
            toRef(obj),
            m.selector.mkString,
            params,
            Name(m.declaringClass.constantPoolName().mkString),
            m.signature().mkString,
            m.declaringClass.isInterface)
          val assOut = if (m.returnType != null && m.returnType != TypeBinding.VOID) {
            val out = vs.portInstances find (_.name == Name("return")) get;
            Assign(toRef(out), invoke)
          } else invoke
          List(assOut, Assign(toRef(thisOut), toRef(obj)))
        case LiteralExprType =>
          val o = LiteralExprType.outPort(vs)
          val c = vs.params.headOption match {
            case Some((t, v: String)) =>
              o.finalTpe match {
                case primitives.Boolean => Const(v.toBoolean, primitives.Boolean)
                case primitives.Byte => Const(v.toByte, primitives.Byte)
                case primitives.Short => Const(v.toShort, primitives.Short)
                case primitives.Int => Const(v.toInt, primitives.Int)
                case primitives.Long => Const(v.dropRight(1).toLong, primitives.Long)
                case primitives.Float => Const(v.toFloat, primitives.Float)
                case primitives.Double => Const(v.toDouble, primitives.Double)
                case str => Const(v, str) // String
              }
            case _ => Const(0, primitives.Byte)
          }
          List(Assign(toRef(o), c))
        case u: UnaryExprType =>
          val (a, o) = u.unaryPortInstancesOf(vs)
          List(
            u match {
              case c: CastExprType => Assign(toRef(o), cast(a.finalTpe, o.finalTpe, toRef(a)))
              case NotExprType => Assign(toRef(o), Not(toRef(a), a.finalTpe.asInstanceOf[PrimitiveJavaType]))
              case MinusExprType => Assign(toRef(o), Minus(toRef(a), a.finalTpe.asInstanceOf[PrimitiveJavaType]))
            })
        case s: BinExprType =>
          val (a, b, o) = s.binaryPortInstancesOf(vs)
          val aTree = toRef(a)
          val bTree = toRef(b)
          val etpe = a.finalTpe.asInstanceOf[PrimitiveJavaType] // is it safe to pick a?
          val eTree = s match {
            case ShiftLeftExprType => ShiftLeft(aTree, bTree, etpe)
            case ShiftRightExprType => ShiftRight(aTree, bTree, etpe)
            case UShiftRightExprType => UShiftRight(aTree, bTree, etpe)
            case AndExprType => And(aTree, bTree, etpe)
            case OrExprType => Or(aTree, bTree, etpe)
            case XorExprType => Xor(aTree, bTree, etpe)
            case AddExprType => Add(aTree, bTree, etpe)
            case SubExprType => Sub(aTree, bTree, etpe)
            case MulExprType => Mul(aTree, bTree, etpe)
            case DivExprType => Div(aTree, bTree, etpe)
            case RemExprType => Rem(aTree, bTree, etpe)
            case LtExprType => Lt(aTree, bTree, etpe)
            case LeExprType => Le(aTree, bTree, etpe)
            case GtExprType => Gt(aTree, bTree, etpe)
            case GeExprType => Ge(aTree, bTree, etpe)
            case EqExprType => Eq(aTree, bTree, etpe)
            case NeExprType => Ne(aTree, bTree, etpe)
          }
          List(Assign(toRef(o), eTree))
      }
      // propagate outputs
      val outs = for {
        (from, to) <- connections.flow;
        if from.valSymbol == vs;
        a <- execConnection((from, to))
      } yield a
      ins ::: invoke ::: outs.toList
    }
    val invokes = block.executionOrder flatMap { runOne }
    val localsDecl = localsMap map {
      case (a, i) =>
        (a.valSymbol.name.str + "_" + a.name.str, a.finalTpe.asInstanceOf[JavaType].descriptor, i)
    } toList;
    Method(Name("contents"), "()V", invokes, localsDecl)
  }
  def box(p: PrimitiveJavaType, t: Tree) =
    InvokeStatic(
      "valueOf",
      List(t),
      p.boxedName,
      "(" + p.descriptor + ")L" + p.boxedName.internal + ";")

  def unbox(c: ClassJavaType, t: Tree): (PrimitiveJavaType, Tree) = {
    val unboxedTpe = primitives.getUnboxedType(c).get
    import primitives._
    (unboxedTpe,
      Invoke(t,
        unboxedTpe.boxMethod,
        List(),
        c.name,
        "()" + unboxedTpe.descriptor,
        false))
  }
  def cast(from: Type, to: Type, t: Tree): Tree = {
    (from, to) match {
      case (from: PrimitiveJavaType, to: ClassJavaType) =>
        box(from, t)
      case (from: ClassJavaType, pt: PrimitiveJavaType) =>
        val (pf, unboxed) = unbox(from, t)
        primitiveCast(pf, pt, unboxed)
      case (pf: PrimitiveJavaType, pt: PrimitiveJavaType) =>
        primitiveCast(pf, pt, t)
      case (from: JavaType, to: JavaType) => t // already checked
    }
  }

  def createLocalsMap(bs: BoxTypeSymbol) = {
    var locals = 1; // 0 for "this"
    bs.block.valsAlphabeticOrder flatMap { v =>
      v.tpe match {
        case b: BoxTypeSymbol => List()
        case e: ExprType =>
          v.portInstances map { pi =>
            (pi -> { val l = locals; locals = locals + pi.finalTpe.javaSize; l })
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