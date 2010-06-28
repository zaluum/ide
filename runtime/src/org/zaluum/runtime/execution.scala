package org.zaluum.runtime
import scala.collection.mutable.{Map,Set}

import java.util.concurrent._
import scala.concurrent.JavaConversions._
import scala.concurrent.{ops,TaskRunner,SyncVar}

sealed abstract class ValueChange{ 
  def fqName:String
}
case class PushValue(fqName:String, value:Any) extends ValueChange
case class ForceValue(fqName:String, value:Any) extends ValueChange
case class UnforceValue(fqName:String) extends ValueChange

trait Model {
  def create(m : MainBox) : Unit
}

class MainBox extends ComposedBox(name="",parent=null) {
  val director = new EventDirector(this)
  override lazy val fqName:String = ""
  override private[runtime] def add(port:Port[_]) = error ("Port cannot be added")
  override def recursiveQueue():Unit = {}
}
import Debug2Model._

class Process (val time:Time) {
  var root : Option[MainBox] = None
  
  def run {
    time.updateNow
    for (r<-root) {
      r.director.run(this)
    }
    time.commit
  }
  def load (model:Model){
    root = Some(new MainBox)
    model.create(root.get)
    def visit(b:Box) : Unit = b match{
      case c:ComposedBox =>
        c.children.values foreach {child =>visit(child)}
        c.recursiveQueue
      case _ => b.recursiveQueue
    }
    visit(root.get)
    run
  }
  def push(values:List[ValueChange]) {
    for (v <- values;
     port <- findPort(v.fqName)){
       v match {
         case PushValue(_,v) => 
           //port.v = v
           //propagate
           port.box.recursiveQueue
         case ForceValue(_,v) => 
           //port.v = v
           port.box.recursiveQueue
         case UnforceValue(_) => /*unforce*/
       }
    }
  }
  def wakeup(boxes:List[Box]){
    boxes.foreach { b=> b.recursiveQueue}
    run
  }
  def toDModel(fqName:String) : serial.ModelProtos.ModelFragment = {
    findComposed(fqName) match {
      case Some(c:ComposedBox) =>
        val fqName = if (c.parent==null) "" else c.parent.fqName
        serial.ModelProtos.ModelFragment.newBuilder
        .setFragment(c.cproto).setFqName(fqName).build
      case None => error("fixme")
    }
  }
  def debugData(fqName:String)= {
    findComposed(fqName) match {
      case Some(c:ComposedBox) =>
        val d = serial.ModelProtos.DebugValue.newBuilder
        d.setFqName(fqName);
        for (b<- c.children.values; 
          p<- b.ports.values){
            d.addValue(serial.ModelProtos.PortValue.newBuilder
              .setBox(b.name)
              .setPort(p.name)
              .setValue(p.v.toString))
        }
        d.build
      case None => error("fixme") 
    }
  }
  private def boxFqNameToList(fqName:String) = (List() ++fqName.split("/")) filter {_.size!=0}
  private def portFqNameToList(fqName:String) :(List[String],Option[String]) = { 
    val boxList = boxFqNameToList(fqName)
    if (boxList.length>0){
      val portSplit = boxList.last.split("#")
      if (portSplit.length==2)
        (boxList.dropRight(1) ::: List(portSplit.head),Some(portSplit.last))
      else (boxList,None)
    }else
      (boxList,None)
  }
  def findComposed(fqName : String) : Option[ComposedBox] =  root flatMap{_.find(boxFqNameToList(fqName))}
  def findPort(fqName:String) : Option[Port[_]]= root flatMap { r=>
    val (box, portO) = portFqNameToList(fqName)
    portO flatMap {port=> r.find(box) flatMap {b=> b.ports.get(port)}}
  }
    
//  def debugRun() =   while (!eventQueue.isEmpty) process(eventQueue.take())
}
trait Time {
  def commit 
  def queue(b : Box, t: Long)
  def updateNow : Unit
  def nowNano : Long
}
