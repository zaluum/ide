package org.zaluum.runtime

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
  def fqName : String
}
case class Slot(pos:Int, left:Boolean)

trait VPort extends FQNamable with Subject {
  type B <: VBox
  def slot:Slot
  def ttype : String 
  def link:String
  def vbox : B
  def in:Boolean
}
trait VBox extends Resizable with Subject with FQNamable{
  type P <: VPort
  type C <: ComposedVBox
  type W <: VWire
  def ports : Set[P]
  def parent : C
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