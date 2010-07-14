package org.zaluum.drivers.local
import org.zaluum.runtime.{Driver,Sink,Source,DefaultSource,Setup,Util, Activate}
import javax.swing.{JFrame,WindowConstants}
import java.awt.event.{KeyListener,KeyEvent}
import se.scalablesolutions.akka.actor.ActorRef
import scala.collection.mutable.{Map}
import scala.collection.immutable.{Map => IMap}

class SwingKeyboardDriver(setup:Setup) extends Driver {
	
  class KeyboardSource(val upVal : Int, val downVal : Int) extends DefaultSource[Int] 
	
  setup.registerDriver("swingkey", this)
	val sources = Map[Char,KeyboardSource]()
	var jframe : JFrame =_
	
	def start(realtime:ActorRef) {
		jframe = new JFrame()
		jframe.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
		jframe.setVisible(true)
		jframe.addKeyListener(new KeyListener{
			def keyTyped(e:KeyEvent){}
			def changeValue(e:KeyEvent, up : Boolean) {
				sources.get(e.getKeyChar) foreach {
				  s => s.write(if (up) s.upVal else s.downVal)
				  realtime ! Activate(List()++s.boxes)
				}
			}
			def keyPressed(e: KeyEvent) = changeValue(e,true)
			def keyReleased(e: KeyEvent) = changeValue(e,false)
		})
	}
	def stop {
		jframe.setVisible(false)
	}

	def getKeySource(c: Char, upVal : Int, downVal:Int) : Source[Int] = { 
		Util.cache(c,sources){new KeyboardSource(upVal,downVal)} 
	}
	def begin() {
	  sources.values foreach { _.commit }
	}
	def commit(){	}
}