package org.zaluum.nide.zge

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import scala.annotation.tailrec
import scala.collection.mutable.Map
import org.eclipse.swt.dnd.ByteArrayTransfer
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.TransferData
import org.zaluum.nide.compiler.Block
import org.zaluum.nide.compiler.ConnectionDef
import org.zaluum.nide.compiler.ConnectionEnd
import org.zaluum.nide.compiler.EditTransformer
import org.zaluum.nide.compiler.MapTransformer
import org.zaluum.nide.compiler.Name
import org.zaluum.nide.compiler.Namer
import org.zaluum.nide.compiler.Parser
import org.zaluum.nide.compiler.Point
import org.zaluum.nide.compiler.PortDef
import org.zaluum.nide.compiler.PortRef
import org.zaluum.nide.compiler.SelectionSubject
import org.zaluum.nide.compiler.Serializer
import org.zaluum.nide.compiler.Template
import org.zaluum.nide.compiler.Tree
import org.zaluum.nide.compiler.ValDef
import org.zaluum.nide.compiler.ValRef
import org.zaluum.nide.compiler.Vector2
import org.zaluum.nide.protobuf.ZaluumProtobuf
import org.zaluum.nide.compiler.Param

case class Clipboard(valDefs: List[ValDef], ports: List[PortDef], connections: List[ConnectionDef]) {
  def isEmpty: Boolean = valDefs.isEmpty && ports.isEmpty
  def renameRelocate(baseNamer: Namer, toPoint: Point, gui: Boolean) = {
    val positions = if (gui) {
      valDefs.view.flatMap(v ⇒ v.bounds).map { _._1 }
    } else
      valDefs.view.map(_.pos) ++ ports.view.map(_.inPos)
    val minPos = if (positions.isEmpty) Vector2(0, 0) else positions.min.toVector.negate
    val deltaCommon = (minPos + toPoint.toVector)
    val deltaGUI = if (gui) deltaCommon else Vector2(10, 10)
    val delta = if (gui) Vector2(10, 10) else deltaCommon
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
      val moved =
        v.bounds match {
          case Some((p, d)) ⇒
            val newPos = p + deltaGUI
            v.copy(params = v.updatedBounds(newPos, d), pos = v.pos + delta)
          case None ⇒
            v.copy(pos = v.pos + delta)
        }
      moved.copy(
        name = Name(rename(v.name.str)),
        typeName = Name(newNames.getOrElse(v.typeName.str, v.typeName.str)))
    }
    val newPorts = for (p ← ports) yield {
      p.copy(
        name = Name(rename(p.name.str)),
        inPos = p.inPos + delta)
    }
    // Connections. we only have ends PortRef(ValRef, name) or EmptyTree
    val newConnections = for (c ← connections) yield {
        def renameRef(t: Option[ConnectionEnd]) = t match {
          case Some(p: PortRef) ⇒
            val vr = p.fromRef.asInstanceOf[ValRef]
            Some(p.copy(fromRef = vr.copy(name = Name(newNames(vr.name.str)))))
          case None ⇒ None
        }

      val res = ConnectionDef(
        renameRef(c.a),
        renameRef(c.b),
        c.points map { p ⇒ p + delta })
      res
    }
    Clipboard(newVals, newPorts, newConnections)
  }
  def pasteCommand(c: ContainerItem, currentMouseLocation: Point, gui: Boolean): MapTransformer = {
    var renamed: Clipboard = null
    new EditTransformer() {
      val trans: PartialFunction[Tree, Tree] = {
        case t: Template if t == c.template ⇒
          val tpe = t.sym
          val bl = c.block
          renamed = renameRelocate(bl.sym, currentMouseLocation, gui)
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
    val valDefs = rawValDefs.filterNot { v ⇒ rawValDefs exists { _.deepContains(v) } }
    val portDefs = rawPorts.filterNot(p ⇒ rawValDefs exists { _.deepContains(p) })
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
  this: ItemViewer ⇒
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