package org.zaluum.ide
import org.eclipse.jface.viewers.CellEditor
import org.eclipse.swt.widgets.Text
import org.eclipse.jface.resource.ImageRegistry
import org.eclipse.gef.tools.CellEditorLocator
import org.eclipse.draw2d.Figure
import org.eclipse.jface.fieldassist.IContentProposalProvider
import org.eclipse.jface.fieldassist.IContentProposal
import scala.collection.JavaConversions._
import scala.collection.mutable._

object Utils {

  def loadIcons(ir : ImageRegistry, base : Class[_], keys : String*) = {
    keys.foreach(k => { loadImage(ir, k + "_16", base); loadImage(ir, k + "_32", base) })
  }
  
  def loadImage(ir : ImageRegistry, key : String, base : Class[_]) = {
    import org.eclipse.jface.resource.ImageDescriptor
    if (ir.getDescriptor(key) == null) {
      ir.put(key, ImageDescriptor.createFromURL(base.getResource(key + ".png")));
    }
  }
  
}

class TextEditorLocator(label:Figure) extends CellEditorLocator {
  override def relocate(celleditor : CellEditor) = {
    val text : Text = celleditor.getControl.asInstanceOf[Text]
    val rect = label.getClientArea
    val trim = text.computeTrim(0, 0, 0, 0);
    label.translateToAbsolute(rect);
    rect.translate(trim.x, trim.y); rect.width += trim.width; rect.height += trim.height;
    text.setBounds(rect.x, rect.y, rect.width, rect.height);    
  } 
}

class EditCPP(val c : Array[String]) extends IContentProposalProvider() {
  override def getProposals(contentsProposal : String, position : Int) = 
    Array.tabulate(c.size)(i=> new ContentProposal(i,c(i)))
}

class ContentProposal(index : Int, text : String) extends IContentProposal {
  override def getContent() : String = { text }
  override def getCursorPosition() : Int = { index }
  override def getLabel() : String = { text }
  override def getDescription() : String = { 
      if(text.startsWith("@")) "Connect to outer port " + text.substring(1) else "Connect to neighbor port " + text
  }
}

class RichCast(val a:Any) {
  def castOption[A](implicit m:Manifest[A]) = 
    if (m.erasure.isInstance(a)) Some(a.asInstanceOf[A]) else None
}
object RichCast {
  implicit def optionCast(a:Any) = new RichCast(a)   
}