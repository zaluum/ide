package org.zaluum.runtime
import serial.ModelProtos._

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

trait PFQNameble extends FQNamable{
  def name_=(s:String)
  def uniquenessSet : Iterable[FQNamable] = List.empty
  def toValidStr (str :String) = {
    val s = str.filter(_.isLetterOrDigit) 
    if (s.length==0) "A" else s
  }
  def uniqueName(str:String)={
    val valid = toValidStr(str)
    var candidate = valid
    var i = 0
    while(uniquenessSet.exists(_.name==candidate)) {
      i+=1
      candidate = valid + i
    } 
    name = candidate
  }  
}

class PBox extends VBox with PFQNameble {
  type B = PBox
  type C = ComposedPBox
  type W = PWire
  type P = PPort
  var parent : C = _
  var ports = Set[PPort]()
  override var name = ""
  def fqName = ""  
  override def uniquenessSet = if (parent==null) List() else parent.boxes
  def toProto = {
     val b = Box.newBuilder
    b.setType(BoxType.SCRIPT)
    b.setBounds((pos,size))
    b.setId(name)
    implicit val o = Ordering.by((_:PPort).name)
    val oports = (List() ++ ports).sorted
    oports foreach {p=> b.addPort(p.toProto) }
    b
  }
}
class PPort extends VPort with PFQNameble{
  type B = PBox
  var vbox : PBox = _
  var ttype = ""
  var slot = Slot(0,true)
  var in = true
  var link = ""
  def fqName = ""
  override var name = ""
  override def uniquenessSet = vbox.ports
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
  var connections = Set[PWire]()
  var boxes = Set[PBox]()
  override def toProto = {
    val box = super.toProto
    box.setType(BoxType.COMPOSED);
    val sorted = (List()++boxes).sorted(Ordering.by((_:PBox).name));
    sorted foreach {c => box.addChild(c.toProto)}
    val wsorted = (List()++connections).sorted(Ordering.by((_:PWire).from.name));
    wsorted foreach {w => box.addWire(w.toProto(this))}
    box
  }
}
import scala.collection.mutable.ArrayBuffer
class PWire extends VWire {
  type P = PPort
  var bendpoints = List[Bendpoint]()
  var from :PPort= _
  var to : PPort= _ 
  def toProto(comp:ComposedPBox) = {
    val line = Line.newBuilder()
    for (bend <- bendpoints) {
      line.addBendpoint(serial.ModelProtos.Bendpoint.newBuilder.setP1(
          bend.a).setP2(bend.b).setWeight(1.0f))
    }
    def pname(p:PPort) =  (if (p.vbox == comp) "#" + p.vbox.name else p.vbox.name) + "#" + p.name
    line.setFrom( pname(from)).setTo(pname(to))    
  }
}
import com.google.protobuf.TextFormat
import java.io.InputStreamReader
import com.google.common.base.Charsets
import serial.ModelProtos
import scala.collection.JavaConversions._

object Deserialize {
  def deserialize(in:java.io.InputStream) : PModel = {
    val boxb = Box.newBuilder();
    TextFormat.merge(new InputStreamReader(in, Charsets.UTF_8), boxb);
    PModel(deserializeComposed(boxb.build()));
  }
  import BoxType._
  def deserialize(b:ModelProtos.Box) : PBox = b.getType match {
    case COMPOSED => deserializeComposed(b)
    case _ => deserializeScript(b)
  }
  def fillBox(b:ModelProtos.Box, r:PBox){
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
  def wire(boxes : Iterable[PBox], ports:Iterable[PPort], w: ModelProtos.Line) : PWire ={
    val from = w.getFrom()
    val to = w.getTo()
    var pfrom : PPort = null
    var pto : PPort = null
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
