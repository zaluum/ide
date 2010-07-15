package org.zaluum.drivers.local
import org.zaluum.runtime.{Driver,DefaultSource,Source,Setup,Util, Activate}
import se.scalablesolutions.akka.actor.{ActorRef,Actor}
import scala.collection.mutable.{Map}
import scala.collection.immutable.{Map => IMap}
import org.eclipse.swt.widgets.{Display,Shell, Listener, Event}
import org.eclipse.swt.SWT

class SWTKeyboardDriver(setup:Setup) extends Driver{
	class KeyboardSource(val upVal : Double, val downVal : Double) extends DefaultSource[Double] 
	setup.registerDriver("swtkey", this)
	val sources = Map[Char,KeyboardSource]()
	private var shell : Shell = _
	def start(realtime:ActorRef) {
		Actor.spawn {
  		val display = new Display
  		def changeValue(c:Char, up : Boolean) {
  			sources.get(c) foreach {
  				s => 
  				  s.write(if (up) s.upVal else s.downVal)
  				  realtime ! Activate(List() ++ s.boxes)
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
  		sources.values foreach { s => s.v  = s.upVal }
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

	def getKeySource(c: Char, upVal : Double, downVal:Double) : Source[Double] = { 
	  
		Util.cache(c,sources){println("creating swt key " + upVal + " " + downVal);new KeyboardSource(upVal,downVal)} 
	}
	def begin(){
	  sources.values foreach { _.commit }
	}
	def commit(){}
}