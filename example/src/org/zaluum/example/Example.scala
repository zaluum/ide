package org.zaluum.example

import org.zaluum.runtime._
import java.util.concurrent._
class SumBox(name:String,parent:ComposedBox) extends Box(name,parent){
	val a = InPort("a",0)
	val b = InPort("b",0)
	val s = OutPort("s",0)
	
	override def act(process:Process):Unit = {
		s.v = a.v+b.v
		process.reschedule(this, 100)
	}
}
class CBox(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
	val director = new EventDirector(this)
	val o = OutPort("o",0)
  val s1 = new SumBox("S1",this)
	val s2 = new SumBox("S2",this)
	val sg = new SumBox("SG",this)
	s1.s connect sg.a
	s2.s connect sg.b
  sg.s connect o

}
class UBox(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
  val director = new EventDirector(this)
  val s = new CBox("CBox",this)
  val o = OutPort("o",0)
  s.o connect o
}
class Example extends Model{
  override def create(main:MainBox) : Unit = {
    new UBox("UBox",main)
  }
}
