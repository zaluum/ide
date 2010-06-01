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

abstract class VPort(var slot:Slot) extends Evaluable with FQNamable with Interactive with Subject {
  def ttype : String
  def vbox : VBox
}
abstract class VBox extends Interactive 
                                    with Resizable with Subject with FQNamable{
  def ports : Iterable[VPort] 
  def parent : ComposedVBox
  def slotUsed(s : Slot) = ports exists(_.slot == s) 
}
case class Point(x:Int, y:Int)
case class Bendpoint(a: Point, b: Point)
abstract class VWire extends Subject{
  def from : VPort
  def to : VPort
  def bendpoints : List[Bendpoint]
}
abstract class ComposedVBox extends VBox {
  def boxes : Iterable[VBox]
  def connections : Set[VWire]
}
case class VModel(root:ComposedVBox)
