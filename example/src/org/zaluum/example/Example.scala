package org.zaluum.example

import org.zaluum.runtime._
import java.util.concurrent._

class Squarewave(period: Int,name:String, parent:ComposedBox) extends Box(name,parent) {
  val o = OutPort("o",0)
  override def act(process:Process):Unit = {
    o.v = if (o.v==0) 1 else 0
    process.time.queue(this, period)
  }
}

class Sum(name:String,parent:ComposedBox) extends Box(name,parent){
	val a = InPort("a",0)
	val b = InPort("b",0)
	val c = InPort("c",0)
	val o = OutPort("o",0)	
	override def act(process:Process) = o.v = a.v+b.v+c.v
}

class SquareSum(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
	val director = new EventDirector(this)
	val o = OutPort("o",0)
	val sq1 = new Squarewave(200,"SQ1",this)
	val sq2 = new Squarewave(250,"SQ2",this)
	val sq3 = new Squarewave(250,"SQ3",this)
  val sum = new Sum("SUM",this) with Resizable 
	sq1.o connect sum.a
	sq2.o connect sum.b
	sq3.o connect sum.c
  sum.o connect o
}
class MultiSum(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
  val director = new EventDirector(this)
  val SS1 = new SquareSum("SS1",this) 
  val SS2 = new SquareSum("SS2",this)
  val SS3 = new SquareSum("SS3",this) 
}
class Example extends Model{
  override def create(main:MainBox) : Unit = {
    new MultiSum("MultiSum",main)
  }
}
