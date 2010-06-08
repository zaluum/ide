package org.zaluum.runtime
import serial.ModelProtos._
import scala.collection.mutable.Set

object ProtoConversions {
  implicit def toPoint(point:(Int,Int)) = {
    serial.ModelProtos.Point.newBuilder.setX(point._1).setY(point._2)
  }
  implicit def toRectangle(p:((Int,Int),(Int,Int))) = {
    val b =Rectangle.newBuilder;
    b.setLeftUp(p._1)
    b.setRightDown((p._1._1+p._2._1, p._1._2+p._2._2))
  }
}
import ProtoConversions._
class PBox extends VBox {
  override var parent : ComposedVBox = _
  var ports = Set[VPort]()
  override var name = ""
  def fqName = ""  
  def toProto = {
     val b = Box.newBuilder
    b.setType(BoxType.SCRIPT)
    b.setBounds((pos,size))
    b.setId(name)
    implicit val o = Ordering.by((_:VPort).name)
    val oports = (List() ++ ports).sorted
    oports foreach {p=> b.addPort(p.asInstanceOf[PPort].toProto) }
    b
  }
}
class PPort extends VPort {
  override var vbox : VBox = _
  override var ttype = ""
  override var slot = Slot(0,true)
  override var in = true
  override var link = ""
  def fqName = ""
  override var name = ""
  def toProto = {
     Port.newBuilder()
      .setName(name)
      .setIn(in)
      .setDirect(false)
      .setLeft(slot.left)
      .setSlot(slot.pos)
      .setType(ttype)
      .setLabel(link)
      .setPosition((0,0))
  }
}
class ComposedPBox extends PBox with ComposedVBox{
  var connections = Set[VWire]()
  var boxes = Set[VBox]()
  override def toProto = {
    val box = super.toProto
    box.setType(BoxType.COMPOSED);
    val sorted = (List()++boxes).sorted(Ordering.by((_:VBox).name));
    sorted foreach {c => box.addChild(c.asInstanceOf[PBox].toProto)}
    val wsorted = (List()++connections).sorted(Ordering.by((_:VWire).from.name));
    wsorted foreach {w => box.addWire(w.asInstanceOf[PWire].toProto(this))}
    box
  }
}
import scala.collection.mutable.ArrayBuffer
class PWire extends VWire  {
  var bendpoints = List[Bendpoint]()
  var from :VPort= _
  var to : VPort= _ 
  def toProto(comp:ComposedPBox) = {
    val line = Line.newBuilder()
    for (bend <- bendpoints) {
      line.addBendpoint(serial.ModelProtos.Bendpoint.newBuilder.setP1(
          bend.a).setP2(bend.b).setWeight(1.0f))
    }
    def pname(p:VPort) =  (if (p.vbox == comp) "#" + p.vbox.name else p.vbox.name) + "#" + p.name
    line.setFrom( pname(from)).setTo(pname(to))    
  }
}
import com.google.protobuf.TextFormat
import java.io.InputStreamReader
import com.google.common.base.Charsets
import serial.ModelProtos
import scala.collection.JavaConversions._
object Deserialize {
  def deserialize(in:java.io.InputStream) : VModel= {
    val boxb = Box.newBuilder();
    TextFormat.merge(new InputStreamReader(in, Charsets.UTF_8), boxb);
    new VModel(deserialize(boxb.build()).asInstanceOf[ComposedVBox]);
  }
  import BoxType._
  def deserialize(b:ModelProtos.Box) : VBox = b.getType match {
    case COMPOSED => deserializeComposed(b)
    case _ => deserializeScript(b)
  }
  def fillBox(b:ModelProtos.Box, r:VBox){
    r.name =b.getId
    val lu = b.getBounds.getLeftUp
    val bd = b.getBounds.getRightDown
    
    r.pos = (lu.getX,lu.getY)
    r.size = (bd.getX-lu.getX, bd.getY-lu.getY)
    for (p<-b.getPortList) {
      val port =deserialize(p)
      port.vbox = r
      r.ports += port
    }
  }
  def deserializeComposed(boxp:ModelProtos.Box) = {
    val c =new ComposedPBox()
    fillBox(boxp,c)
    for (ch <- boxp.getChildList()) {
      val chbox = deserialize(ch);
      chbox.parent = c
      c.boxes += chbox
    }
    for (w <- boxp.getWireList()) {
      c.connections += wire(c.boxes,c.ports, w)
    }
    c
  }
  def wire(boxes : Iterable[VBox], ports:Iterable[VPort], w: ModelProtos.Line) : VWire ={
    val from = w.getFrom()
    val to = w.getTo()
    var pfrom : VPort = null
    var pto : VPort = null
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
    val res = new PWire()
    res.to = pto
    res.from = pfrom
    res.bendpoints = List() ++ deserialBendpoints(w);
    res
  }

  def deserialize(p: ModelProtos.Port) = {
    val port =new PPort
    port.name=p.getName
    port.ttype = p.getType
    //port.diresetDirect(p.getDirect());
    port.in = p.getIn
    port.slot = Slot(p.getSlot,p.getLeft)
    if (p.hasLabel)
      port.link = p.getLabel
    port
  }
  def deserialBendpoints(w:ModelProtos.Line) = {
    def deserialize(p:ModelProtos.Bendpoint) =  Bendpoint((p.getP1.getX, p.getP1.getY),(p.getP2.getX, p.getP2.getY)) 
    w.getBendpointList map {deserialize(_)}
  }

  def deserializeScript(pb:ModelProtos.Box) = {
    val b = new PBox()
    fillBox(pb,b)
    b
  }
}
