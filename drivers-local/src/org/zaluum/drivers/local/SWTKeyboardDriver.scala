package org.zaluum.drivers.local
import org.zaluum.runtime.{Driver,Sink,Source,Setup,Util, SourceValuesEvent}
import se.scalablesolutions.akka.actor.{ActorRef,Actor}
import scala.collection.mutable.{Map}
import scala.collection.immutable.{Map => IMap}
import org.eclipse.swt.widgets.{Display,Shell, Listener, Event}
import org.eclipse.swt.SWT

class SWTKeyboardDriver(setup:Setup) extends Driver{
	class KeyboardSource(val upVal : Int, val downVal : Int) extends Source[Int] 
	setup.registerDriver("swtkey", this)
	val sources = Map[Char,KeyboardSource]()
	private var shell : Shell = _
	def start(realtime:ActorRef) {
		Actor.spawn {
  		val display = new Display
  		def changeValue(c:Char, up : Boolean) {
  			sources.get(c) foreach {
  				s => realtime ! SourceValuesEvent(IMap((s,if (up) s.upVal else s.downVal)))
  			}
  		}
  		var lastKeyPressed : Int = -1
  		display.addFilter(SWT.KeyDown, new Listener{
  			def handleEvent(e:Event) {
  				if (lastKeyPressed != e.keyCode)
  					changeValue(e.character,false)
  				lastKeyPressed = e.keyCode
  			}
  		});
  		display.addFilter(SWT.KeyUp, new Listener{
  			def handleEvent(e:Event) {
  				lastKeyPressed = -1
  				changeValue(e.character,true)
  			}
  		});
  		realtime ! SourceValuesEvent(IMap() ++( sources.values map { s => s -> s.upVal }) )
  		shell = new Shell(display)
  		shell.setSize(50,50)
  		shell.open;
  		while (!shell.isDisposed ()) {
  			if (!display.readAndDispatch ()) display.sleep ();
  		}
  		display.dispose ();
		}
	}
	def stop {
		shell.close
	}

	def getKeySource(c: Char, upVal : Int, downVal:Int) : Source[Int] = { 
	  
		Util.cache(c,sources){println("creating swt key " + upVal + " " + downVal);new KeyboardSource(upVal,downVal)} 
	}
	def commit(){}
}