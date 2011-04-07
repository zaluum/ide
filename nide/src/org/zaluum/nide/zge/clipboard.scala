package org.zaluum.nide.zge

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import org.zaluum.nide.protobuf.BoxFileProtos
import org.eclipse.swt.dnd.TransferData
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.ByteArrayTransfer
import scala.collection.mutable.Buffer
import org.zaluum.nide.compiler._

case class Clipboard(boxes: List[BoxDef], valDefs: List[ValDef], ports: List[PortDef], connections: List[ConnectionDef]) {
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
    // Connections we only have ends PortRef(ValRef, name) or EmptyTree
    val newConnections = for (c ← connections) yield {
      def renameRef(t: Tree) = t match {
        case p: PortRef ⇒
          val vr = p.fromRef.asInstanceOf[ValRef]
          p.copy(fromRef = vr.copy(name = Name(newNames(vr.name.str)))) 
        case EmptyTree ⇒ EmptyTree
      }
      ConnectionDef(renameRef(c.a), renameRef(c.b), c.points map { p ⇒ p + minPos + pos.toVector })
    }
    Clipboard(newBoxes, newVals, newPorts, newConnections)
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
            transformTrees(b.connections) ++ renamed.connections,
            transformTrees(b.junctions))
      }
    }
  }
}

object Clipboard {
  def createFromSelection(selection: Set[SelectionSubject]): Clipboard = {
    val valDefs = selection.collect { case v: ValDef ⇒ v }
    val ports = selection.collect { case p: PortDef⇒ p }
    def hasVal(o: Option[ValRef]) = o match {
      case Some(vr) ⇒ valDefs.exists { _.name == vr.name }
      case None ⇒ false
    }
    /*def hasPort(o: Option[PortRef]) = o match {
      case Some(pr) ⇒ ports.exists(_.name == pr.name)
      case None ⇒ false
    }*/
    // Only connections valDefs TODO improve
    val unfilteredConnections = selection.collect {
      case c: ConnectionDef ⇒ c
      case l: LineSelectionSubject ⇒ l.c
    }
    val connections = unfilteredConnections.filter { c ⇒
      def validEnd(t: Tree) = (hasVal(c.valRef(t)) /*&& hasPort(c.portRef(t))*/ ) || t == EmptyTree
      validEnd(c.a) && validEnd(c.b)
    }
    println ("connections" + unfilteredConnections)
    println ("filter connections " + connections)
    def localDecl(v: ValDef) = v.tpe match {
      case NoSymbol ⇒ None
      case b: BoxTypeSymbol ⇒ if (b.isLocal) Some(b.decl.asInstanceOf[BoxDef]) else None
    }
    val boxes = for (v ← valDefs; b ← localDecl(v)) yield b
    Clipboard(boxes.toList, valDefs.toList, ports.toList, connections.toList)
  }
}
object ClipTransfer extends ByteArrayTransfer {
  val typeName = "boxTransfer:" + System.currentTimeMillis() + ":" + ClipTransfer.hashCode
  val typeID = Transfer.registerType(typeName);
  override protected def getTypeIds = Array(typeID)
  override protected def getTypeNames = Array(typeName)

  override protected def javaToNative(data: Object, transferData: TransferData) =
    data match {
      case c: BoxFileProtos.Clipboard ⇒
        val b = new ByteArrayOutputStream()
        c.writeDelimitedTo(b);
        super.javaToNative(b.toByteArray, transferData)
    }

  override protected def nativeToJava(transferData: TransferData) = {
    val bytes = super.nativeToJava(transferData).asInstanceOf[Array[Byte]]
    BoxFileProtos.Clipboard.parseDelimitedFrom(new ByteArrayInputStream(bytes));
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
    Option(eclip.getContents(ClipTransfer).asInstanceOf[BoxFileProtos.Clipboard]).
      map { c ⇒ Parser.parse(c) }
  } catch { case e ⇒ None }
}