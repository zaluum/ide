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
  val s1 = new SumBox("S1",this) with Resizable 
	val s2 = new SumBox("S2",this)
	val sg = new SumBox("SG",this)
	s1.pos = (100,100);	s1.size = (100,100)
	s1.s connect sg.a
	s2.s connect sg.b
  sg.s connect o

}
class IBox(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
  val director = new EventDirector(this)
  val i = InPort("i",0)
}
class UBox(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
  val director = new EventDirector(this)
  val s = new CBox("CBox",this)  with Resizable
  val s2 = new IBox("IBox",this) with Resizable
  val o = OutPort("o",0)
  s.pos= (100,100)  ; s.size = (100,50)
  s2.pos=(300,100) ; s2.size =(100,50)
  s.o connect s2.i
  //s.o connect o
  
}
class Example extends Model{
  override def create(main:MainBox) : Unit = {
    new UBox("UBox",main)
  }
}
