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
  def fqName : String
}

case class Slot(pos:Int, left:Boolean)

trait VPort extends FQNamable with Subject {
  type B <: VBox
  def slot:Slot
  def slot_=(s:Slot) { error("RO")}
  def ttype : String 
  def ttype_=(s:String) { error("RO")}
  def link:String
  def link_=(s:String) {error("RO")}
  def vbox : B
  def vbox_=(v:B) { error("RO")}
  def in:Boolean
  def in_=(b:Boolean) {error("RO")}
}
trait VBox extends Resizable with Subject with FQNamable{
  type P <: VPort
  type C <: ComposedVBox
  type W <: VWire
  def ports : Set[P]
  def parent : C
  def parent_= (p:C) { error("RO") }
  def slotUsed(s : Slot) = ports exists(_.slot == s) 
}
case class Bendpoint(a: (Int,Int), b: (Int,Int))
trait VWire extends Subject{
  type P <: VPort
  def from : P
  def to : P
  def bendpoints : List[Bendpoint]
}
trait ComposedVBox extends VBox {
  type B <: VBox
  def boxes : Set[B]
  def connections : Set[W]
}

abstract class BaseVModel extends Subject {
  type C <: ComposedVBox
  val root : C
}
case class VModel(val root:ComposedVBox) extends BaseVModel{
  type C = ComposedVBox
}
case class PModel(val root:ComposedPBox) extends BaseVModel{
  type C = ComposedPBox
}