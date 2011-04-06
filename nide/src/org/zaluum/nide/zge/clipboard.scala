package org.zaluum.nide.zge

import org.zaluum.nide.protobuf.BoxFileProtos
import org.eclipse.swt.dnd.TransferData
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.ByteArrayTransfer
import scala.collection.mutable.Buffer
import org.zaluum.nide.compiler._

case class Clipboard(boxes: List[BoxDef], valDefs: List[ValDef], ports: List[PortDef], connections: List[ConnectionDef]) {

  def positions = (valDefs.view.map(_.pos) ++ ports.view.map(_.inPos))
  val minPos = if (positions.isEmpty) Vector2(0, 0) else positions.min.toVector.negate
  def renameRelocate(baseNamer: Namer, pos: Point) = {
    import scala.collection.mutable.Map
    val newNames = Map[String, String]()
    val namer = new Namer() {
      def usedNames = baseNamer.usedNames ++ newNames.values
    }
    def rename(oldName: String): String = {
      val newName = namer.freshName(oldName)
      newNames += (oldName -> newName)
      newName
    }
    val newBoxes = for (b ← boxes) yield { b.copy(name = Name(rename(b.name.str))) }
    val newVals = for (v ← valDefs) yield {
      v.copy(
        name = Name(rename(v.name.str)),
        typeName = Name(newNames.getOrElse(v.typeName.str, v.typeName.str)),
        pos = v.pos + minPos + pos.toVector)
    }
    val newPorts = for (p ← ports) yield {
      p.copy(
        name = Name(rename(p.name.str)),
        inPos = p.inPos + minPos + pos.toVector)
    }
    // TODO Connections
    Clipboard(newBoxes, newVals, newPorts, List())
  }
  def pasteCommand(c: ContainerItem, currentMouseLocation: Point): MapTransformer = {
    new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case b: BoxDef if b == c.boxDef ⇒
          val tpe = b.symbol.asInstanceOf[BoxTypeSymbol]
          val renamed = renameRelocate(tpe, currentMouseLocation)
          BoxDef(b.name, b.superName, guiSize = b.guiSize, b.image,
            transformTrees(b.defs) ++ renamed.boxes,
            transformTrees(b.vals) ++ renamed.valDefs,
            transformTrees(b.ports) ++ renamed.ports,
            transformTrees(b.connections),
            transformTrees(b.junctions))
      }
    }
  }
}

object Clipboard {
  def createFromSelection(selection: Set[SelectionSubject]): Clipboard = {
    var valDefs = selection.collect { case v: ValDef ⇒ v }
    var ports = selection.collect { case p: PortDef ⇒ p }
    var connections = selection.collect { case c: ConnectionDef ⇒ c }
    def localDecl(v: ValDef) = v.tpe match {
      case NoSymbol ⇒ None
      case b: BoxTypeSymbol ⇒ if (b.isLocal) Some(b.decl.asInstanceOf[BoxDef]) else None
    }
    var boxes = for (v ← valDefs; b ← localDecl(v)) yield b
    Clipboard(boxes.toList, valDefs.toList, ports.toList, connections.toList)
  }
}
object ClipTransfer extends ByteArrayTransfer {
  val typeName = "boxTransfer:" + System.currentTimeMillis() + ":" + ClipTransfer.hashCode
  val typeID = Transfer.registerType(typeName);
  override protected def getTypeIds = Array(typeID)
  override protected def getTypeNames = Array(typeName)

  override protected def javaToNative(data: Object, transferData: TransferData) =
    data match { case c: BoxFileProtos.Clipboard ⇒ super.javaToNative(c.toByteArray, transferData) }

  override protected def nativeToJava(transferData: TransferData) = {
    val bytes = super.nativeToJava(transferData).asInstanceOf[Array[Byte]]
    BoxFileProtos.Clipboard.newBuilder.mergeFrom(bytes).build
  }
}

trait ClipboardViewer {
  this: TreeViewer ⇒
  def updateClipboard = {
    val clip = Serializer.proto(Clipboard.createFromSelection(selection.currentSelected))
    val eclip = new org.eclipse.swt.dnd.Clipboard(display)
    eclip.setContents(Array(clip), Array(ClipTransfer))
  }
  import scala.util.control.Exception._
  def getClipboard: Option[Clipboard] = try {
    val eclip = new org.eclipse.swt.dnd.Clipboard(display)
    Option(eclip.getContents(ClipTransfer).asInstanceOf[BoxFileProtos.Clipboard]).
      map { c ⇒ Parser.parse(c) }
  } catch { case e ⇒ None }
}