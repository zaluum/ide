package org.zaluum.runtime
import scala.collection.mutable.{Map,Set}
import scala.collection.immutable.{Map => IMap,Set => ISet}
import scala.collection.immutable.{Set => ISet}
import java.util.concurrent._
import scala.concurrent.JavaConversions._
import scala.concurrent.{ops,TaskRunner,SyncVar}

sealed abstract class ValueChange{ 
  def fqName:String
}
case class PushValue(fqName:String, value:Any) extends ValueChange
case class ForceValue(fqName:String, value:Any) extends ValueChange
case class UnforceValue(fqName:String) extends ValueChange

trait ModelBuilder {
  def create(setup:Setup) : MainBox
}

class MainBox extends ComposedBox(name="",parent=null) {
  override lazy val fqName:String = ""
  override private[runtime] def add(port:Port[_]) = error ("Port cannot be added")
  override def activate():Unit = {}
}
import Debug2Model._

class Process (begin : ()=> Unit, end : ()=> Unit) {
  var root : Option[MainBox] = None
  
  def run {
    begin()
    for (r<-root) {
      r.director.run(this)
    }
    end()
  }
  def load (mainBox : MainBox){
  	root = Some(mainBox)
    def visit(b:Box) : Unit = b match{
      case c:ComposedBox =>
        c.children.values foreach {child =>visit(child)}
        c.init(this)
        c.activate
      case _ => 
        b.init(this) 
        b.activate
    }
    visit(root.get)
    run
  }
  private def dopush(port:Port[Any], v:Any, force:Boolean){
      if (Util.checkAssignable(v, port.manifest)){
        if (force) 
          port.forced = Some(v)
        else {port.forced = None; port.v = v}
        port.changed
      } 
    }
  def push(values:List[ValueChange]) {
    for (vc <- values; port <- findPort(vc.fqName)){
      val aport = port.asInstanceOf[Port[Any]]
      vc match {
        case PushValue(_,newv) => dopush(aport,newv,false)
        case ForceValue(_,newv) => dopush(aport,newv,true)
        case UnforceValue(_) => aport.forced = None; aport.changed
      }
    }
    run
  }
  def wakeup(boxes:List[Box]){
    boxes.foreach { b=> b.activate}
    run
  }
  def toDModel(fqName:String) : Option[serial.ModelProtos.ModelFragment] = {
    findComposed(fqName) match {
      case Some(c:ComposedBox) =>
        val fqName = if (c.parent==null) "" else c.parent.fqName
        Some(serial.ModelProtos.ModelFragment.newBuilder
            .setFragment(c.proto).setFqName(fqName).build
            )
      case None => None
    }
  }
  def debugData(fqNames:ISet[String])= {
    val dr = serial.ModelProtos.DebugResponse.newBuilder
    for (fqName <- fqNames){
      findBox(fqName) map { b=>
        val dv = serial.ModelProtos.DebugValue.newBuilder
        dv.setBoxFQ(fqName);
        for (p<- b.ports.values){
            dv.addPortValue(serial.ModelProtos.PortValue.newBuilder
              .setPortName(p.name)
              .setValue(p.v.toString))
        }
        dr.addValue(dv);
      }
    }
    dr.build
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
  
  def findBox(fqName:String) : Option[Box] = root flatMap{_.find(boxFqNameToList(fqName))}

  def findComposed(fqName : String) : Option[ComposedBox] = findBox(fqName) match {
    case Some(c:ComposedBox) => Some(c)
    case None => None
  }

  def findPort(fqName:String) : Option[Port[_]]= root flatMap { r=>
    val (boxl, portO) = portFqNameToList(fqName)
    portO flatMap {port=>  r.find(boxl) flatMap {b=> b.ports.get(port)} }
  }
    
//  def debugRun() =   while (!eventQueue.isEmpty) process(eventQueue.take())
}
trait Time {
  def commit 
  def queue(b : Box, t: Long)
  def updateNow : Unit
  def nowNano : Long
}
