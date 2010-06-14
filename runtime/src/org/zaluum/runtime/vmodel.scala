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
case class Bendpoint(a: (Int,Int), b: (Int,Int))
abstract class VisualModel{
  type B <: VBox
  type P <: VPort
  type C <: ComposedVBox
  type W <: VWire
  trait VPort extends FQNamable with Subject {
    def slot:Slot
    def ttype : String 
    def link:String
    def vbox : B
    def in:Boolean
  }
  trait VBox extends Resizable with Subject with FQNamable{
    def ports : Set[P]
    def parent : C
    def slotUsed(s : Slot) = ports exists(_.slot == s) 
  }
  trait VWire extends Subject{
    def from : P
    def to : P
    def bendpoints : List[Bendpoint]
  }
  trait ComposedVBox extends VBox {
    def boxes : Set[B]
    def connections : Set[W]
  }
  trait VModel extends Subject{
    def root:C 
  }
}