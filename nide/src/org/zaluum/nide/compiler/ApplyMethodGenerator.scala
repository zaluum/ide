package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding

class MainThreadMethodGenerator(bs: BoxTypeSymbol, thread: ZThread) extends MethodGenerator(bs, thread) {
  def apply(): Method = {
    val localsDecl = localsMap map {
      case (pi, i) ⇒
        (pi.fqName.str, pi.tpe.asInstanceOf[JavaType].descriptor, i)
    } toList;
    val invokes = runThread(thread)
    val assign = assignFlowInConnections(bs.thisVal, bs.blocks.head) // assign outputs
    val ret = bs.returnPort match {
      case Some(r) ⇒
        val pi = bs.thisVal.findPortInstance(r).get
        Return(toRef(pi), pi.tpe)
      case None ⇒
        Return
    }
    val annotation = bs.argsInOrder.map { _.name }
    Method(bs.methodSelector, bs.methodSignature, invokes ::: (assign :+ ret), localsDecl, Some(annotation))
  }
}
class RunnableMethodGenerator(bs: BoxTypeSymbol, thread: ZThread) extends MethodGenerator(bs, thread) {
  def apply(): Method = {
    val invokes = runThread(thread)
    val localsDecl = localsMap map {
      case (pi, i) ⇒
        val name = if (pi.valSymbol == bs.thisVal) pi.name.str
        else pi.valSymbol.fqName.str + "_" + pi.name.str
        (name, pi.tpe.asInstanceOf[JavaType].descriptor, i)
    } toList;
    Method(Name("run"), "()V", invokes :+ Return, localsDecl, None)
  }
  override def thisRef = {
    Select(This, FieldRef(Name("$this0"), bs.tpe.fqName.descriptor, thread.fqName(bs.fqName)))
  }
}
class MethodGenerator(val bs: BoxTypeSymbol, thread: ZThread) extends GeneratorHelpers {
  val localsMap = createLocalsMap()

  def execConnection(c: (PortInstance, Set[PortInstance])) = {
    val (out, ins) = c
    ins.toList map { in ⇒ assign(in, out) }
  }
  def assign(to: PortInstance, from: PortInstance): Assign = {
    Assign(toRef(to), cast(from.tpe, to.tpe, toRef(from)))
  }
  def toRef(pi: PortInstance): Ref = pi.internalStorage match {
    case StorageLocal ⇒ LocalRef(localsMap(pi), pi.tpe.name)
    case StorageValField ⇒
      Select(
        valRef(pi.valSymbol),
        FieldRef(pi.name, pi.tpe.name.descriptor, pi.valSymbol.tpe.fqName))
    case StorageJoinField ⇒
      Select(thisRef,
        FieldRef(pi.joinfqName, pi.tpe.name.descriptor, bs.fqName))
  }
  def runThread(thread: ZThread): List[Tree] = {
    thread.instructions flatMap { vs ⇒
      val outs = for {
        (from, to) ← thread.blockSymbol.connections.flow; // order
        if from.valSymbol == vs;
        a ← execConnection((from, to))
      } yield a
      runOne(vs, thread) ::: outs.toList
    }
    // FIXME wait for spawned threads
  }
  def assignFlowInConnections(vs: ValSymbol, bl: BlockSymbol): List[Tree] = {
    for (ps ← vs.portSides; if (ps.flowIn); val pi = ps.pi) yield {
      bl.connections.connectedFrom.get(pi) match {
        case Some((o, blame)) ⇒ assign(pi, o)
        case None             ⇒ Assign(toRef(pi), Const(0, pi.tpe))
      }
    }
  }

  def runOne(vs: ValSymbol, thread: ZThread): List[Tree] = {
    // joins
    val joins = vs.join.toList map { toJoin ⇒
      Invoke(
        semaphoreRef(toJoin),
        "acquire",
        List(),
        TreeToClass.semaphoreClassName,
        "()V",
        interface = false)
    }
    // propagate inputs
    val propagate = assignFlowInConnections(vs, thread.blockSymbol)
    import primitives._
      def invokeHelper(vs: ValSymbol, m: MethodBinding, invoke: Tree): Tree = // TODO find a better place
        if (m.returnType != null && m.returnType != TypeBinding.VOID) {
          val out = vs.portInstances find (_.name == Name("return")) get;
          Assign(toRef(out), invoke)
        } else invoke

    val insn: List[Tree] = vs.tpe match {
      case vbs: BoxTypeSymbol ⇒
        val tpe = vbs.fqName
        val args = vbs.argsInOrder map { ps ⇒ toRef(vs.findPortInstance(ps).get) }
        val invoke = Invoke(
          valRef(vs),
          vbs.methodSelector.str,
          args,
          tpe,
          vbs.methodSignature,
          interface = false)
        val res = vbs.returnPort map { p ⇒
          val pi = vs.findPortInstance(p).get
          Assign(toRef(pi), invoke)
        } getOrElse (invoke)
        List(res)
      case WhileExprType ⇒
        List(While(
          runThread(vs.blocks.head.threads(0)),
          toRef(WhileExprType.endPort(vs))))
      case IfExprType ⇒
        List(
          If(
            toRef(IfExprType.condPort(vs)),
            runThread(vs.blocks(0).threads(0)),
            runThread(vs.blocks(1).threads(0))))
      case ArrayExprType ⇒
        val index = ArrayExprType.indexPort(vs)
        val thisPort = ArrayExprType.thisPort(vs)
        val thisOutPort = ArrayExprType.thisOutPort(vs)
        val aPort = ArrayExprType.aPort(vs)
        val oPort = ArrayExprType.outPort(vs)
          def arrayRef = ArrayRef(index = toRef(index), arrRef = toRef(thisPort), arrTpe = aPort.tpe)
          def load = Assign(toRef(oPort), arrayRef)
          def store = Assign(arrayRef, toRef(aPort))
          def thisOut = Assign(toRef(thisOutPort), toRef(thisPort))
        thread.blockSymbol.connections.connectedFrom.get(aPort) match {
          case Some(_) ⇒ // do store
            List(
              store,
              Assign(toRef(oPort), toRef(aPort)),
              thisOut)
          case None ⇒ List(
            load,
            thisOut)
        }
      case InvokeExprType ⇒
        val m = vs.info.asInstanceOf[MethodBinding]
        val obj = InvokeExprType.thisPort(vs)
        val thisOut = InvokeExprType.thisOutPort(vs) // XXX optimize and use only 1 var
        val params = vs.portSides filter { ps ⇒ ps.inPort && ps.pi != obj } sortBy { _.pi.name.str } map { ps ⇒ toRef(ps.pi) }
        val invoke = Invoke(
          toRef(obj),
          m.selector.mkString,
          params,
          Name(m.declaringClass.constantPoolName().mkString),
          m.signature().mkString,
          m.declaringClass.isInterface)
        List(invokeHelper(vs, m, invoke), Assign(toRef(thisOut), toRef(obj)))
      case NewExprType ⇒
        val m = vs.info.asInstanceOf[MethodBinding]
        val thiz = NewExprType.thisPort(vs)
        val params = vs.portSides filter { ps ⇒ ps.inPort } sortBy { _.pi.name.str } map { ps ⇒ toRef(ps.pi) }
        List(
          Assign(toRef(thiz),
            New(Name(m.declaringClass.constantPoolName.mkString),
              params,
              m.signature().mkString)))
      case NewArrayExprType ⇒
        val thiz = NewArrayExprType.thisPort(vs)
        val ab = thiz.tpe.asInstanceOf[ArrayType]
        val dimPorts = vs.portInstances.filter { pi ⇒ pi.dir == In && pi.name.str.startsWith("d") }.sortBy { _.name.str.drop(1).toInt } // XXX ugly
        List(
          Assign(
            toRef(thiz),
            NewArray(dimPorts.map { toRef(_) }, ab.of)))
      case InvokeStaticExprType ⇒
        val m = vs.info.asInstanceOf[MethodBinding]
        // TODO share with invoke
        val params = vs.portSides filter { ps ⇒ ps.inPort } sortBy { _.pi.name.str } map { ps ⇒ toRef(ps.pi) }
        val invoke =
          InvokeStatic(
            meth = m.selector.mkString,
            param = params,
            fromClass = Name(m.declaringClass.constantPoolName.mkString),
            descriptor = m.signature.mkString)
        List(invokeHelper(vs, m, invoke))
      case FieldExprType ⇒
        val f = vs.info.asInstanceOf[FieldBinding]
        val a = FieldExprType.aPort(vs)
        val o = FieldExprType.outPort(vs)
        val obj = FieldExprType.thisPort(vs)
        val thisOut = FieldExprType.thisOutPort(vs)
          def fieldRef = Select(
            toRef(obj),
            FieldRef(
              Name(f.name.mkString),
              f.`type`.signature.mkString,
              Name(f.declaringClass.constantPoolName.mkString)))
          def store = Assign(fieldRef, toRef(a))
          def load = Assign(toRef(o), fieldRef)
          def storeThisOut = Assign(toRef(thisOut), toRef(obj))
        thread.blockSymbol.connections.connectedFrom.get(a) match {
          case Some(_) ⇒ // do store
            List(
              store,
              Assign(toRef(o), toRef(a)),
              storeThisOut)
          case None ⇒ List(
            load,
            storeThisOut)
        }
      case StaticFieldExprType ⇒ // share with field
        val f = vs.info.asInstanceOf[FieldBinding]
        val a = StaticFieldExprType.aPort(vs)
        val o = StaticFieldExprType.outPort(vs)
          def fieldRef = FieldStaticRef(
            Name(f.name.mkString),
            f.`type`.signature.mkString,
            Name(f.declaringClass.constantPoolName.mkString))
          def store = Assign(fieldRef, toRef(a))
          def load = Assign(toRef(o), fieldRef)
        thread.blockSymbol.connections.connectedFrom.get(a) match {
          case Some(_) ⇒ List(store, Assign(toRef(o), toRef(a)))
          case None    ⇒ List(load)
        }
      case LiteralExprType ⇒
        val o = LiteralExprType.outPort(vs)
        val c = vs.params.headOption match {
          case Some((t, v: String)) ⇒
            o.tpe match {
              case primitives.Boolean ⇒ Const(v.toBoolean, primitives.Boolean)
              case primitives.Byte    ⇒ Const(v.toByte, primitives.Byte)
              case primitives.Short   ⇒ Const(v.toShort, primitives.Short)
              case primitives.Int     ⇒ Const(v.toInt, primitives.Int)
              case primitives.Long    ⇒ Const(v.dropRight(1).toLong, primitives.Long)
              case primitives.Float   ⇒ Const(v.toFloat, primitives.Float)
              case primitives.Double  ⇒ Const(v.toDouble, primitives.Double)
              case str                ⇒ Const(v, str) // String
            }
          case _ ⇒ Const(0, primitives.Byte)
        }
        List(Assign(toRef(o), c))
      case u: UnaryExprType ⇒
        val (a, o) = u.unaryPortInstancesOf(vs)
        List(
          u match {
            case c: CastExprType ⇒ Assign(toRef(o), cast(a.tpe, o.tpe, toRef(a)))
            case NotExprType     ⇒ Assign(toRef(o), Not(toRef(a), a.tpe.asInstanceOf[PrimitiveJavaType]))
            case MinusExprType   ⇒ Assign(toRef(o), Minus(toRef(a), a.tpe.asInstanceOf[PrimitiveJavaType]))
          })
      case s: BinExprType ⇒
        val (a, b, o) = s.binaryPortInstancesOf(vs)
        val aTree = toRef(a)
        val bTree = toRef(b)
        val etpe = a.tpe.asInstanceOf[PrimitiveJavaType] // is it safe to pick a?
        val eTree = s match {
          case ShiftLeftExprType   ⇒ ShiftLeft(aTree, bTree, etpe)
          case ShiftRightExprType  ⇒ ShiftRight(aTree, bTree, etpe)
          case UShiftRightExprType ⇒ UShiftRight(aTree, bTree, etpe)
          case AndExprType         ⇒ And(aTree, bTree, etpe)
          case OrExprType          ⇒ Or(aTree, bTree, etpe)
          case XorExprType         ⇒ Xor(aTree, bTree, etpe)
          case AddExprType         ⇒ Add(aTree, bTree, etpe)
          case SubExprType         ⇒ Sub(aTree, bTree, etpe)
          case MulExprType         ⇒ Mul(aTree, bTree, etpe)
          case DivExprType         ⇒ Div(aTree, bTree, etpe)
          case RemExprType         ⇒ Rem(aTree, bTree, etpe)
          case LtExprType          ⇒ Lt(aTree, bTree, etpe)
          case LeExprType          ⇒ Le(aTree, bTree, etpe)
          case GtExprType          ⇒ Gt(aTree, bTree, etpe)
          case GeExprType          ⇒ Ge(aTree, bTree, etpe)
          case EqExprType          ⇒ Eq(aTree, bTree, etpe)
          case NeExprType          ⇒ Ne(aTree, bTree, etpe)
        }
        List(Assign(toRef(o), eTree))
    }
    // release sem
    val release = if (vs.isJoinPoint) {
      List(releaseSem(vs))
    } else List()
    // forks
    val forks = for (toFork ← vs.fork.toList) yield {
      executeThread(toFork)
    }
    joins ::: propagate ::: insn ::: release ::: forks
  }
  def executeThread(thread: ZThread) = {
    InvokeStatic("execute",
      List(threadRef(thread)),
      Name(classOf[org.zaluum.basic.Zaluum].getName),
      "(Ljava/lang/Runnable;)V")
  }
  def releaseSem(vs: ValSymbol) =
    Invoke(
      semaphoreRef(vs),
      "release",
      List(),
      TreeToClass.semaphoreClassName,
      "()V",
      interface = false)
  def acquireSem(vs: ValSymbol) =
    Invoke(
      semaphoreRef(vs),
      "acquire",
      List(),
      TreeToClass.semaphoreClassName,
      "()V",
      interface = false)
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
      case (from: PrimitiveJavaType, to: ClassJavaType) ⇒
        box(from, t)
      case (from: ClassJavaType, pt: PrimitiveJavaType) ⇒
        val (pf, unboxed) = unbox(from, t)
        primitiveCast(pf, pt, unboxed)
      case (pf: PrimitiveJavaType, pt: PrimitiveJavaType) ⇒
        primitiveCast(pf, pt, t)
      case (from: JavaType, to: JavaType) ⇒ t // already checked
    }
  }
  def createLocalsMap(): Map[PortInstance, Int] = {
    var locals = 1; // 0 for "this"
    var localsMap = Map[PortInstance, Int]()
      def createLocal(pi: PortInstance) {
        if (!localsMap.contains(pi)) {
          println("creating local " + pi)
          localsMap += (pi -> locals)
          locals = locals + pi.tpe.javaSize
        }
      }

    bs.argsInOrder map { ps ⇒ createLocal(bs.thisVal.findPortInstance(ps).get) }
    println(deepValSymbols(bs.thisVal))
    for {
      vs ← deepValSymbols(bs.thisVal)
      pi ← vs.portInstances
      if (pi.internalStorage == StorageLocal)
    } createLocal(pi)
    localsMap
  }
  def primitiveCast(from: PrimitiveJavaType, to: PrimitiveJavaType, t: Tree) = {
    import primitives._
    (from, to) match {
      case (Float, Char)   ⇒ I2C(F2I(t))
      case (Double, Char)  ⇒ I2C(D2I(t))
      case (Int, Char)     ⇒ I2C(t)
      case (Short, Char)   ⇒ I2C(t)
      case (Byte, Char)    ⇒ I2C(t)
      case (Long, Char)    ⇒ I2C(L2I(t))

      case (Char, Float)   ⇒ I2F(t)
      case (Short, Float)  ⇒ I2F(t)
      case (Int, Float)    ⇒ I2F(t)
      case (Byte, Float)   ⇒ I2F(t)
      case (Double, Float) ⇒ D2F(t)
      case (Long, Float)   ⇒ L2F(t)

      case (Float, Byte)   ⇒ I2B(F2I(t))
      case (Double, Byte)  ⇒ I2B(D2I(t))
      case (Int, Byte)     ⇒ I2B(t)
      case (Short, Byte)   ⇒ I2B(t)
      case (Char, Byte)    ⇒ I2B(t)
      case (Long, Byte)    ⇒ I2B(L2I(t))

      case (Byte, Double)  ⇒ I2D(t)
      case (Char, Double)  ⇒ I2D(t)
      case (Short, Double) ⇒ I2D(t)
      case (Int, Double)   ⇒ I2D(t)
      case (Float, Double) ⇒ F2D(t)
      case (Long, Double)  ⇒ L2D(t)

      case (Byte, Short)   ⇒ I2S(t)
      case (Char, Short)   ⇒ I2S(t)
      case (Int, Short)    ⇒ I2S(t)
      case (Double, Short) ⇒ I2S(D2I(t))
      case (Long, Short)   ⇒ I2S(L2I(t))
      case (Float, Short)  ⇒ I2S(F2I(t))

      case (Double, Int)   ⇒ D2I(t)
      case (Float, Int)    ⇒ F2I(t)
      case (Long, Int)     ⇒ L2I(t)

      case (Int, Long)     ⇒ I2L(t)
      case (Char, Long)    ⇒ I2L(t)
      case (Byte, Long)    ⇒ I2L(t)
      case (Short, Long)   ⇒ I2L(t)
      case (Double, Long)  ⇒ D2L(t)
      case (Float, Long)   ⇒ F2L(t)
      case _               ⇒ t
    }
  }
}