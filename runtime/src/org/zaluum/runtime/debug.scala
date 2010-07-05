package org.zaluum.runtime
import serial.ModelProtos
object Debug2Model extends VisualModel{
  type B = DBox
  type C = ComposedDBox
  type W = DWire
  type P = DPort
  class DBox(
      val name:String,
      val fqName:String, 
      val ports:Set[DPort], 
      var parent:ComposedDBox) extends VBox
  
  class DPort(
      val name:String,
      val fqName:String,
      val slot:Slot,
      val ttype : String, 
      val link:String,
      val in:Boolean
    ) extends VPort {
      @volatile var value :String= "?"
      var vbox : DBox = _
    }
  class DWire(val from:DPort, val to:DPort, val bendpoints:List[Bendpoint]) extends VWire 
  class ComposedDBox(name:String,fqName:String,ports:Set[DPort], parent:ComposedDBox,
      val connections:Set[DWire], val boxes:Set[DBox]) 
    extends DBox(name,fqName,ports,parent) with ComposedVBox
  
  object Deserialize {
    import scala.collection.JavaConversions._
    import ModelProtos.BoxType._
    def deserialize(m:ModelProtos.ModelFragment) = {
      deserializeComposed(m.getFragment, m.getFqName)
    }
    def deserializeBox(b:ModelProtos.Box, fqName : String) : DBox = b.getType match {
      case COMPOSED => deserializeComposed(b,fqName)
      case _ => deserializeScript(b,fqName)
    }
    def deserializeComposed(boxp:ModelProtos.Box, fqName:String) = {
      val myFqName = fqName + "/" + boxp.getId
      val boxes = for (ch <- boxp.getChildList()) 
        yield deserializeBox(ch,myFqName);
      val ports = for (p <- boxp.getPortList) 
        yield port(p,myFqName)
      val wires = for (w <- boxp.getWireList()) 
        yield wire(boxes,ports, w)
      val r = new ComposedDBox(boxp.getId,myFqName, Set()++ports,null,Set()++wires, Set()++boxes)
      r.boxes foreach { _.parent  =r}
      r.ports foreach { _.vbox = r}
      r
    }
    def wire(boxes : Iterable[DBox], ports:Iterable[DPort], w: ModelProtos.Line) : DWire ={
      val from = w.getFrom()
      val to = w.getTo()
      var pfrom : DPort = null
      var pto : DPort = null
      import com.google.common.base.Splitter
      def getExternal(port:String) = {
         val names = List() ++ Splitter.on('#').split(port);
         val box = names.get(0);
         val portName = names.get(1);
         val child = boxes find (_.name==box) getOrElse(error("Cannot find box" + box))
         child.ports find (_.name==portName) getOrElse(error("Cannot find port" + portName))
      }
      def getInternal(port:String)= ports find (_.name == port.substring(1)) getOrElse (error("Cannot find port" + port))
      def isInternal(port:String) = port.startsWith("#")
      def isExternal(port:String) = !isInternal(port)
      if (isInternal(from) && isInternal(to)) {
        pfrom = getInternal(from);
        pto = getInternal(to);
      } else if (isInternal(from) && isExternal(to)) {
        pfrom = getInternal(from);
        pto = getExternal(to);
      } else if (isExternal(from) && isInternal(to)) {
        pfrom = getExternal(from);
        pto = getInternal(to);
      } else { // external external
        pfrom = getExternal(from);
        pto = getExternal(to);
      }
      assert(pto != null)
      assert(pfrom!=null)
      new DWire(pfrom,pto, List() ++ deserialBendpoints(w))
    }
  
    def port(p: ModelProtos.Port, fqName : String) = new DPort(
          p.getName, 
          fqName + "#" + p.getName,
          Slot(p.getSlot, p.getLeft),
          p.getType,
          if (p.hasLabel) p.getLabel else "",
          p.getIn)
    
    def deserialBendpoints(w:ModelProtos.Line) = {
      def deserialize(p:ModelProtos.Bendpoint) =  Bendpoint((p.getP1.getX, p.getP1.getY),(p.getP2.getX, p.getP2.getY)) 
      w.getBendpointList map {deserialize(_)}
    }
  
    def deserializeScript(pb:ModelProtos.Box, fqName : String) = {
      val myFQName = fqName + "/" + pb.getId
      val ports = for (p <- pb.getPortList)  
        yield port(p, myFQName)
      val r = new DBox(pb.getId, myFQName, Set()++ports,null)
      r.ports foreach {_.vbox = r}
      r
    }
  }
}