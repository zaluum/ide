package org.zaluum.runtime
import scala.collection.mutable.{Buffer,MultiMap,Set}

trait Subject {
  private var observers = List[Observer]()
  def addObserver(observer: Observer) = observers ::= observer
  def removeObserver(observer: Observer) = observers filterNot (_==observer)
  def notifyObservers = observers foreach (_.receiveUpdate(this))  
}
trait Observer {
    def receiveUpdate(subject: Subject)
}
trait Interactive {
  def onClick() : Unit = {}
  def onDoubleClick() : Unit = {}
}
trait Positional {
  self:Subject =>
  private var _pos : (Int,Int) = (0,0)
  def pos = _pos
  def pos_= (p:(Int,Int)) {_pos=p; notifyObservers}
}
trait Resizable extends Positional {
  self : Subject=>
  private var _size : (Int,Int) = (50,50)
  def size = _size
  def size_= (s : (Int,Int)) {_size=s; notifyObservers}
}
trait FQNamable {
  def name : String
  def name_= (s:String) { error("VO")}
  type Named = { def name:String}
  def uniquenessSet : Iterable[FQNamable] = List.empty
  def uniqueName(str:String)={
    var candidate = str
    var i = 0
    while(uniquenessSet.exists(_.name==candidate)) {
      i+=1
      candidate = str + i
    } 
    name = candidate
  }
  def fqName : String
}
trait Context {
  def valueOf(fqName:String) : String
  def setValue(fqName:String, value:String):Unit
}
trait Evaluable {
  self : FQNamable =>
  def value(c:Context) : String = c.valueOf(fqName)
  def value_=(s:String,c:Context) : Unit = c.setValue(fqName, s)
}

case class Slot(pos:Int, left:Boolean)

trait VPort extends Evaluable with FQNamable with Interactive with Subject {
  def slot:Slot
  def slot_=(s:Slot) { error("RO")}
  def ttype : String 
  def ttype_=(s:String) { error("RO")}
  def link:String
  def link_=(s:String) {error("RO")}
  def vbox : VBox
  def vbox_=(v:VBox) { error("RO")}
  def in:Boolean
  def in_=(b:Boolean) {error("RO")}
}
trait VBox extends Interactive with Resizable with Subject with FQNamable{
  def ports : Set[VPort]
  def parent : ComposedVBox
  def parent_= (p:ComposedVBox) { error("RO") }
  def slotUsed(s : Slot) = ports exists(_.slot == s) 
}
case class Bendpoint(a: (Int,Int), b: (Int,Int))
trait VWire extends Subject{
  def from : VPort
  def to : VPort
  def bendpoints : List[Bendpoint]
}
trait ComposedVBox extends VBox {
  def boxes : Set[VBox]
  def connections : Set[VWire]
}
case class VModel(root:ComposedVBox) extends Subject
