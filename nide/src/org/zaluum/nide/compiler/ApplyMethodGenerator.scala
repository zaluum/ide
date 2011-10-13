package org.zaluum.nide.compiler
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding
import org.eclipse.jdt.internal.compiler.lookup.TypeBinding
import org.eclipse.jdt.internal.compiler.lookup.FieldBinding
import org.eclipse.jdt.internal.compiler.lookup.ArrayBinding
import java.util.concurrent.CyclicBarrier
import scala.collection.mutable.Buffer

class MainThreadMethodGenerator(bs: BoxSymbol) extends MethodGenerator(bs) {
  val block = bs.block
  def withLine[T <: Tree](t: T, line: Int): T = {
    t.line = line; t
  }
  def apply(): Method = {
    val ins = Buffer[Tree]()
    // create arg locals
    bs.argsInOrder map { ps ⇒ locals.createLocal(bs.thisVal.findPortInstance(ps).get) }
    // create val locals
    for (pi ← bs.thisVal.portInstances; if (pi.internalStorage == StorageLocal))
      locals.createLocal(pi)
    //create thread locals
    createThreadLocals(block.mainPath)
    // assign args to fields if needed
    for (arg ← bs.argsInOrder) {
      val pi = bs.thisVal.findPortInstance(arg).get
      if (pi.internalStorage != StorageLocal)
        ins += Assign(toRef(pi), LocalRef(locals(pi), pi.tpe.get.fqName))
    }
    // run block
    ins ++= runBlock(block)
    // assign outputs
    ins ++= assignFlowInConnections(bs.thisVal, block)
    bs.returnPort match {
      case Some(r) ⇒
        val pi = bs.thisVal.findPortInstance(r).get
        ins += Return(toRef(pi), pi.tpe.get.fqName)
      case None ⇒
        ins += Return
    }
    val annotation = bs.argsInOrder.map { _.name }
    Method(bs.methodSelector, bs.methodSignature, ins.toList, locals.localsDecl, Some(annotation))
  }
}
class RunnableMethodGenerator(bs: BoxSymbol, startPath: ExecutionPath) extends MethodGenerator(bs) {
  def apply(): Method = {
    val ins = Buffer[Tree]()
    createThreadLocals(startPath);
    ins ++= runExecutionPath(startPath)
    ins += Return
    Method(Name("run"), "()V", ins.toList, locals.localsDecl, None)
  }
  override def thisRef = {
    Select(This, FieldRef(Name(TreeToClass.enclosingClassFieldName), bs.fqName.descriptor, startPath.fqName(bs)))
  }
}
class MethodLocals {
  type HasType = { def tpe: Option[JavaType]; def fqName: Name }
  var localMap = Map[HasType, Int]()
  var locals = 1; // 0 for "this"
  def apply(t: HasType) = localMap(t)
  def createLocal(ref: HasType) {
    if (!localMap.contains(ref)) {
      localMap += (ref -> locals)
      locals = locals + ref.tpe.get.javaSize
    }
  }
  def localsDecl = {
    localMap.map {
      case (p, i) ⇒
        (p.fqName.str, p.tpe.get.descriptor, i)
    } toList;
  }
}
abstract class MethodGenerator(val bs: BoxSymbol) extends GeneratorHelpers {
  val locals = new MethodLocals

  def createThreadLocals(startPath: ExecutionPath) {
    for (
      ep ← (startPath :: deepChildMainPaths(startPath));
      vs ← ep.instructions;
      pi ← vs.portInstances;
      if (pi.internalStorage == StorageLocal)
    ) locals.createLocal(pi)
  }
  def execConnection(c: (PortInstance, Set[PortInstance])) = {
    val (out, ins) = c
    ins.toList map { in ⇒ assign(in, out) }
  }
  def assign(to: PortInstance, from: PortInstance): Assign = {
    Assign(toRef(to), cast(from.tpe.get, to.tpe.get, toRef(from)))
  }
  def toRef(pi: PortInstance): Ref = pi.internalStorage match {
    case StorageLocal ⇒
      LocalRef(locals(pi), pi.tpe.get.fqName)
    case StorageValField ⇒
      Select(
        valRef(pi.valSymbol),
        FieldRef(pi.name, pi.tpe.get.fqName.descriptor, pi.valSymbol.javaType.fqName))
    case StorageJoinField ⇒
      Select(thisRef,
        FieldRef(pi.joinfqName, pi.tpe.get.fqName.descriptor, bs.fqName))
  }
  def runExecutionPath(execPath: ExecutionPath) = {
    val ins = Buffer[Tree]()
    execPath.instructions foreach { vs ⇒
      ins ++= runOne(vs, execPath)
      for {
        (from, to) ← execPath.blockSymbol.connections.flow;
        if from.valSymbol == vs;
        a ← execConnection((from, to))
      } ins += a
    }
    // can I join the forked threads?
    execPath.instructions.flatMap { _.fork } foreach { forked ⇒
      assert(forked.num != 0)
      ins ++= getFuture(forked)
    }
    ins.toList
  }
  def runBlock(block: BlockSymbol): List[Tree] = {
    val ins = Buffer[Tree]()
    val spawnThreads = block.secondaryPaths.filter(_.forkedBy.isEmpty)
    for (spawn ← spawnThreads) {
      ins += executeThread(spawn)
    }
    ins ++= runExecutionPath(block.mainPath)
    for (spawn ← spawnThreads) {
      ins ++= getFuture(spawn)
    }
    ins.toList
  }
  def assignFlowInConnections(vs: ValSymbol, bl: BlockSymbol): List[Tree] = {
    for (ps ← vs.portSides; if (ps.flowIn); val pi = ps.pi) yield {
      bl.connections.connectedFrom.get(pi) match {
        case Some((o, blame)) ⇒ assign(pi, o)
        case None             ⇒ Assign(toRef(pi), Const(0, pi.tpe.get.fqName))
      }
    }
  }
  def params(vs: ValSymbol, except: PortInstance) =
    vs.portSides filter { ps ⇒ ps.inPort && ps.pi != except } sortBy { _.pi.name.str } map { ps ⇒ toRef(ps.pi) }

  def runOne(vs: ValSymbol, execPath: ExecutionPath): List[Tree] = {
    val ins = Buffer[Tree]()
    // line
    ins += Lbl(vs.decl.line)
    // joins
    vs.join.toList foreach { toJoin ⇒
      ins += acquireSem(toJoin)
    }
    // propagate inputs
    ins ++= assignFlowInConnections(vs, execPath.blockSymbol)
    import primitives._
      def invokeHelper(vs: ValSymbol, m: MethodBinding, invoke: Tree): Tree = // TODO find a better place
        if (m.returnType != null && m.returnType != TypeBinding.VOID) {
          val out = vs.portInstances find (_.name == Name("return")) get;
          Assign(toRef(out), invoke)
        } else invoke

    vs.tpe.foreach {
      _ match {
        case BoxExprType ⇒
          val cl = vs.classinfo.asInstanceOf[ClassJavaType]
          val selector = vs.info.asInstanceOf[String]
          val argsInOrder = vs.ports.values.toList filter { p ⇒ p.dir == In } sortBy { _.name.str }
          val returnPort = vs.ports.values.toList find { p ⇒ p.dir == Out && !p.isField }
          val returnDescriptor = returnPort map { _.tpe.get.fqName.descriptor } getOrElse ("V")
          val methodSignature = "(" + argsInOrder.map { _.tpe.get.fqName.descriptor }.mkString + ")" + returnDescriptor

          val tpe = cl.fqName
          val args = argsInOrder map { ps ⇒ toRef(vs.findPortInstance(ps).get) }
          val invoke = Invoke(
            valRef(vs),
            selector,
            args,
            tpe,
            methodSignature,
            interface = false)
          val res = returnPort map { p ⇒
            val pi = vs.findPortInstance(p).get
            Assign(toRef(pi), invoke)
          } getOrElse (invoke)
          ins += res
        case WhileExprType ⇒
          ins += While(
            runBlock(vs.blocks.head),
            toRef(WhileExprType.endPort(vs)))
        case IfExprType ⇒
          ins +=
            If(
              toRef(IfExprType.condPort(vs)),
              runBlock(vs.blocks(0)),
              runBlock(vs.blocks(1)))
        case ArrayExprType ⇒
          val index = ArrayExprType.indexPort(vs)
          val thisPort = ArrayExprType.thisPort(vs)
          val thisOutPort = ArrayExprType.thisOutPort(vs)
          val aPort = ArrayExprType.aPort(vs)
          val oPort = ArrayExprType.outPort(vs)
            def arrayRef = ArrayRef(index = toRef(index), arrRef = toRef(thisPort), arrTpe = aPort.tpe.get)
            def load = Assign(toRef(oPort), arrayRef)
            def store = Assign(arrayRef, toRef(aPort))
            def thisOut = Assign(toRef(thisOutPort), toRef(thisPort))
          execPath.blockSymbol.connections.connectedFrom.get(aPort) match {
            case Some(_) ⇒ // do store
              ins += store
              ins += Assign(toRef(oPort), toRef(aPort))
            case None ⇒
              ins += load
          }
          ins += thisOut
        case ArrayComposeExprType ⇒
          val size = vs.info.asInstanceOf[Int]
          val out = vs.findPortInstance(ArrayComposeExprType.out).get
          ins +=
            Assign(
              toRef(out),
              NewArray(List(Const(size, primitives.Int.fqName)), vs.classinfo))
          for (i ← 0 until size) {
            val pi = vs.portInstances.find(_.name == Name("element" + i)).get
            ins += Assign(
              ArrayRef(
                index = Const(i, primitives.Int.fqName),
                arrRef = toRef(out),
                arrTpe = pi.tpe.get),
              toRef(pi))
          }
        case InvokeExprType ⇒
          val m = vs.info.asInstanceOf[MethodBinding]
          val obj = InvokeExprType.thisPort(vs)
          val thisOut = InvokeExprType.thisOutPort(vs) // XXX optimize and use only 1 var
          val invoke = Invoke(
            toRef(obj),
            m.selector.mkString,
            params(vs, obj),
            Name(m.declaringClass.constantPoolName().mkString),
            m.signature().mkString,
            m.declaringClass.isInterface)
          ins += invokeHelper(vs, m, invoke)
          ins += Assign(toRef(thisOut), toRef(obj))
        case NewExprType ⇒
          val m = vs.info.asInstanceOf[MethodBinding]
          val thiz = NewExprType.thisPort(vs)
          ins +=
            Assign(toRef(thiz),
              New(Name(m.declaringClass.constantPoolName.mkString),
                params(vs, null),
                m.signature().mkString))
        case NewArrayExprType ⇒
          val thiz = NewArrayExprType.thisPort(vs)
          val ab = thiz.tpe.asInstanceOf[Option[ArrayType]].get
          val dimPorts = vs.portInstances.filter { pi ⇒ pi.dir == In && pi.name.str.startsWith("d") }.sortBy { _.name.str.drop(1).toInt } // XXX ugly
          ins +=
            Assign(
              toRef(thiz),
              NewArray(dimPorts.map { toRef(_) }, ab.of))
        case InvokeStaticExprType ⇒
          val m = vs.info.asInstanceOf[MethodBinding]
          // TODO share with invoke
          val invoke =
            InvokeStatic(
              meth = m.selector.mkString,
              param = params(vs, null),
              fromClass = Name(m.declaringClass.constantPoolName.mkString),
              descriptor = m.signature.mkString)
          ins += invokeHelper(vs, m, invoke)
        case ThisRefExprType ⇒
          val port = ThisRefExprType.thisPort(vs)
          ins += Assign(toRef(port), thisRef)
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
          execPath.blockSymbol.connections.connectedFrom.get(a) match {
            case Some(_) ⇒ // do store
              ins += store
              ins += Assign(toRef(o), toRef(a))
            case None ⇒
              ins += load
          }
          ins += storeThisOut
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
          execPath.blockSymbol.connections.connectedFrom.get(a) match {
            case Some(_) ⇒
              ins += store
              ins += Assign(toRef(o), toRef(a))
            case None ⇒
              ins += load
          }
        case LiteralExprType ⇒
          val o = LiteralExprType.outPort(vs)
          val c = vs.getValue(LiteralExprType.paramDecl) match {
            case Some(v) ⇒
              v.codeGen
            case _ ⇒ new Const(0, primitives.Byte)
          }
          ins += Assign(toRef(o), c)
        case u: UnaryExprType ⇒
          val (a, o) = u.unaryPortInstancesOf(vs)
          ins += (u match {
            case c: CastExprType ⇒ Assign(toRef(o), cast(a.tpe.get, o.tpe.get, toRef(a)))
            case NotExprType     ⇒ Assign(toRef(o), Not(toRef(a), a.tpe.get.asInstanceOf[PrimitiveJavaType]))
            case MinusExprType   ⇒ Assign(toRef(o), Minus(toRef(a), a.tpe.get.asInstanceOf[PrimitiveJavaType]))
          })
        case s: BinExprType ⇒
          val (a, b, o) = s.binaryPortInstancesOf(vs)
          val aTree = toRef(a)
          val bTree = toRef(b)
          val etpe = a.tpe.get.asInstanceOf[PrimitiveJavaType] // is it safe to pick a?
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
          ins += Assign(toRef(o), eTree)
        case o ⇒ throw new RuntimeException("Not implemented " + o)
      }
    }
    // release sem
    if (vs.isJoinPoint)
      ins += releaseSem(vs)
    // forks
    for (toFork ← vs.fork.toList) {
      ins += executeThread(toFork)
    }
    ins.toList
  }
  def executeThread(startPath: ExecutionPath) = {
    Assign(futureRef(startPath),
      InvokeStatic("submit",
        List(threadRef(startPath)),
        Name(classOf[org.zaluum.basic.Zaluum].getName),
        "(Ljava/lang/Runnable;)Ljava/util/concurrent/Future;"))
  }
  def getFuture(startPath: ExecutionPath) = List(
    Invoke(futureRef(startPath), "get", List(), TreeToClass.futureClassName, "()Ljava/lang/Object;", true),
    Pop)
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
        c.fqName,
        "()" + unboxedTpe.descriptor,
        false))
  }

  def cast(from: JavaType, to: JavaType, t: Tree): Tree = {
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