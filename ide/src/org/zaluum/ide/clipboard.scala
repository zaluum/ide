package org.zaluum.ide


import org.eclipse.gef.EditPart;
import org.eclipse.gef.ui.actions.SelectionAction;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.widgets.Event;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import scala.collection.JavaConversions._
import org.zaluum.runtime._
import scala.collection.mutable.Buffer
import Commands._
import RichCast.optionCast
import PersistentModel._
import PersistentEditParts._
abstract class ClipboardAction(part:IWorkbenchPart) extends SelectionAction(part) {
  override protected def calculateEnabled = true // TODO: implement me (mirar que l'editor sigui el mateix?)
  def execute(c:Command) = 
    for (z <- getWorkbenchPart.castOption[ZFileEditor])
      z.editDomain.getCommandStack.execute(c)

  def currentComposed : Option[VisualModel#C] = getWorkbenchPart match {
    case e:ZFileEditor =>  e.modelEditPart.model.currentView match {
        case TopView => None
        case ComposedView(c) => Some(c.asInstanceOf[VisualModel#C])
      }
    case _ => None
  }
    
  override def runWithEvent(event:Event) {runClipboard(new Clipboard(event.display))}
  def runClipboard(clipboard:Clipboard)
 
}
class CopyAction(part : IWorkbenchPart) extends ClipboardAction(part) {

  override def calculateEnabled = true
  override def runClipboard(c:Clipboard) = copy(c)
  protected def  copy(clipboard:Clipboard) = {
    val pack = serial.ModelProtos.Clipboard.newBuilder
    val selectedBoxes = for{
      o<-getSelectedObjects
      e<-o.castOption[BoxEditPartWrite]
    }yield e.model       
    val validBoxes = currentComposed match {
      case Some(p) => selectedBoxes filter {_.parent == p}
      case None => Buffer[PBox]()
    }
    // filter contained wires
    def containedWire(w:VisualModel#W)={
      def containsPort(p:VisualModel#P) = validBoxes.exists(b => b.ports.contains(p))
      containsPort(w.to) && containsPort(w.from)
    }
    val wires = Buffer[PWire]()
    for (p <- currentComposed; 
      w <- p.connections; 
      if containedWire(w)){ 
      pack.addWire(w.asInstanceOf[PWire].toProto(p.asInstanceOf[ComposedPBox]))
      wires +=w.asInstanceOf[PWire]
    }
    //validBoxes.foreach{b=>pack.addBox(b.toProto)}
    val ret= pack.build
    clipboard.setContents(Array(ret), Array(ClipTransfer))
    (validBoxes,wires)
  }
  
  override def init {
    setId(ActionFactory.COPY.getId());
    setText("Copy");
    setToolTipText("Copy");
    val sharedImages = PlatformUI.getWorkbench()
        .getSharedImages();
    setImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
    setDisabledImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_COPY_DISABLED));
    setEnabled(false);
  }
}
class CutAction(part: IWorkbenchPart) extends CopyAction(part){
  override def runClipboard(clipboard:Clipboard) {
    val (validBoxes,wires) = copy(clipboard)
   /* for (c<-currentComposed)
      execute(CutCommand(c,validBoxes,wires))*/
  }
   override def init {
    setId(ActionFactory.CUT.getId());
    setText("Cut");
    setToolTipText("Cut");
    val sharedImages = PlatformUI.getWorkbench()
        .getSharedImages();
    setImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_CUT));
    setDisabledImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_CUT_DISABLED));
    setEnabled(false);
  }
}

class PasteAction(part:IWorkbenchPart) extends ClipboardAction(part) { 
  override def runClipboard(clipboard:Clipboard) {
     for {c<-currentComposed
       clip <- clipboard.getContents(ClipTransfer).castOption[serial.ModelProtos.Clipboard]
     } execute(PasteCommand(c.asInstanceOf[ComposedPBox],clip))
  }
  protected override def init {
    setId(ActionFactory.PASTE.getId());
    setText("Paste");
    setToolTipText("Paste");
    val sharedImages = PlatformUI.getWorkbench()
        .getSharedImages();
    setImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_PASTE));
    setDisabledImageDescriptor(sharedImages
        .getImageDescriptor(ISharedImages.IMG_TOOL_PASTE_DISABLED));
    setEnabled(false);
  }
}
import org.eclipse.swt.dnd.ByteArrayTransfer;
import org.eclipse.swt.dnd.TransferData;

object ClipTransfer extends ByteArrayTransfer {
  val typeName = "boxTransfer:"  + System.currentTimeMillis() + ":" + ClipTransfer.hashCode
  val typeID = Transfer.registerType(typeName);
  override protected def getTypeIds = Array(typeID)
  override protected def getTypeNames = Array(typeName)

  override protected def javaToNative(data : Object, transferData : TransferData) = 
    data match {case c : serial.ModelProtos.Clipboard => super.javaToNative(c.toByteArray, transferData)}

  override protected def nativeToJava(transferData : TransferData) = {
    val bytes = super.nativeToJava(transferData).asInstanceOf[Array[Byte]]
    serial.ModelProtos.Clipboard.newBuilder.mergeFrom(bytes).build
  }
}