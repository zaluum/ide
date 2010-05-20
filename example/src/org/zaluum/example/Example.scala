package org.zaluum.example

import org.zaluum.runtime._

class SumBox(name:String,parent:ComposedBox) extends Box(name,parent){
	val a = InPort("a",0)
	val b = InPort("b",0)
	val s = OutPort("s",0)
	
	def act() = {
		s.v = a.v+b.v
	}
}
class CBox(name:String, parent:ComposedBox) extends ComposedBox(name,parent){
	val director = new CyclicDirector(this)
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
  val s = new CBox("Sum",this)
  val o = OutPort("o",0)
  s.o connect o
}
class Example extends Model{
  override def create(main:MainBox) : Unit = {
    new UBox("Top",main)
  }
}
object Main{
	def main(args: Array[String]) = runAsync
	def debugRun()={
	  val e = new Process()
		val u = new UBox("Top",e.root)
		e.debugRun()
		
    println("****")

    e.queue(new PushInputEvent[Int](u.s.s1.a, 2))
		e.debugRun()
		println("****")
    /*e
    .queue(new PushEvent[Int](u.s.o, 3))
    //e.queue(new PushEvent[Int](u.s.s2.b, ))
    e.debugRun()
    println("****")*/
    println(u.s)
    println(u)

	}
	def runAsync {
	  var u: UBox = null
	  val e = new EventProcessor()
	  e.startAndWait()
	  e.process.queue(LoadEvent(new Model {def create(b:MainBox) = u=new UBox("Top",b)}))
	  e.process.queue(PushInputEvent(u.s.s1.a,0))
	}
}