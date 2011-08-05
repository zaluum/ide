package org.zaluum.nide.zge

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import org.eclipse.swt.dnd.TransferData
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.ByteArrayTransfer
import scala.collection.mutable.Buffer
import org.zaluum.nide.compiler._
import org.zaluum.nide.protobuf.ZaluumProtobuf

case class Clipboard(valDefs: List[ValDef], ports: List[PortDef], connections: List[ConnectionDef]) {
  def isEmpty: Boolean = valDefs.isEmpty && ports.isEmpty
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
    // Connections. we only have ends PortRef(ValRef, name) or EmptyTree
    val newConnections = for (c ← connections) yield {
        def renameRef(t: Option[ConnectionEnd]) = t match {
          case Some(p: PortRef) ⇒
            val vr = p.fromRef.asInstanceOf[ValRef]
            Some(p.copy(fromRef = vr.copy(name = Name(newNames(vr.name.str)))))
          case None ⇒ None
        }

      val res = ConnectionDef(renameRef(c.a), renameRef(c.b), c.points map { p ⇒ p + minPos + pos.toVector })
      res
    }
    Clipboard(newVals, newPorts, newConnections)
  }
  def pasteCommand(c: ContainerItem, currentMouseLocation: Point): MapTransformer = {
    var renamed: Clipboard = null
    new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case t: Template if t == c.template ⇒
          val tpe = t.sym
          val bl = c.block
          renamed = renameRelocate(bl.sym, currentMouseLocation)
          t.copy(blocks = transformTrees(t.blocks),
            ports = transformTrees(t.ports) ++ renamed.ports)
        case bl: Block if bl == c.block ⇒
          bl.copy(valDefs = transformTrees(bl.valDefs) ++ renamed.valDefs,
            connections = transformTrees(bl.connections) ++ renamed.connections,
            junctions = transformTrees(bl.junctions))
      }
    }
  }
}

object Clipboard {
  def createFromSelection(selection: Set[SelectionSubject]): Clipboard = {

    val rawValDefs = selection.collect { case v: ValDef ⇒ v }
    val rawPorts = selection.collect { case p: PortDef ⇒ p }

    // Filter out nested selections 
    val valDefs = rawValDefs.filterNot { v ⇒ rawValDefs exists { _.deepchildren.contains(v) } }
    val portDefs = rawPorts.filterNot(p ⇒ rawValDefs exists { _.deepchildren.contains(p) })
    // Only connections valDefs TODO improve
    val unfilteredConnections = selection.collect {
      case c: ConnectionDef        ⇒ c
      case l: LineSelectionSubject ⇒ l.c
    }
    val connections = unfilteredConnections.filter { c ⇒
        def validEnd(o: Option[ConnectionEnd]) = o match {
          case Some(pr: PortRef) ⇒
            pr.fromRef match {
              case vr: ValRef ⇒ valDefs.exists { _.name == vr.name }
              case _          ⇒ false
            }
          case None ⇒ true
          case _    ⇒ false
        }
      validEnd(c.a) && validEnd(c.b)
    }
    Clipboard(valDefs.toList, portDefs.toList, connections.toList)
  }
}
object ClipTransfer extends ByteArrayTransfer {
  val typeName = "boxTransfer:" + System.currentTimeMillis() + ":" + ClipTransfer.hashCode
  val typeID = Transfer.registerType(typeName);
  override protected def getTypeIds = Array(typeID)
  override protected def getTypeNames = Array(typeName)

  override protected def javaToNative(data: Object, transferData: TransferData) =
    data match {
      case c: ZaluumProtobuf.Clipboard ⇒
        val b = new ByteArrayOutputStream()
        c.writeDelimitedTo(b);
        super.javaToNative(b.toByteArray, transferData)
    }

  override protected def nativeToJava(transferData: TransferData) = {
    val bytes = super.nativeToJava(transferData).asInstanceOf[Array[Byte]]
    ZaluumProtobuf.Clipboard.parseDelimitedFrom(new ByteArrayInputStream(bytes));
  }
}

trait ClipboardViewer {
  this: TreeViewer ⇒
  def updateClipboard = {
    val nclip = Clipboard.createFromSelection(selection.currentSelected)
    if (!nclip.isEmpty) {
      val clip = Serializer.proto(nclip)
      val eclip = new org.eclipse.swt.dnd.Clipboard(display)
      eclip.setContents(Array(clip), Array(ClipTransfer))
    }
  }
  import scala.util.control.Exception._
  def getClipboard: Option[Clipboard] = try {
    val eclip = new org.eclipse.swt.dnd.Clipboard(display)
    Option(eclip.getContents(ClipTransfer).asInstanceOf[ZaluumProtobuf.Clipboard]).
      map { c ⇒ Parser.parse(c) }
  } catch { case e ⇒ None }
}