package org.zaluum.nide.palette
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.lang.Object
import java.lang.System

import org.eclipse.swt.dnd.ByteArrayTransfer
import org.eclipse.swt.dnd.Transfer
import org.eclipse.swt.dnd.TransferData

object PaletteTransfer extends ByteArrayTransfer {
  val typeName = "paletteTransfer:" + System.currentTimeMillis() + ":" + PaletteTransfer.hashCode
  val typeID = Transfer.registerType(typeName);
  override protected def getTypeIds = Array(typeID)
  override protected def getTypeNames = Array(typeName)

  override protected def javaToNative(data: Object, transferData: TransferData) =
    data match {
      case p: PaletteEntry ⇒
        val b = new ByteArrayOutputStream()
        val o = new ObjectOutputStream(b)
        o.writeObject(p)
        super.javaToNative(b.toByteArray, transferData)
    }

  override protected def nativeToJava(transferData: TransferData) = {
    val bytes = super.nativeToJava(transferData).asInstanceOf[Array[Byte]]
    try {
      new ObjectInputStream(new ByteArrayInputStream(bytes)).readObject()
    } catch { case e: Exception ⇒ e.printStackTrace; null }
  }
}
