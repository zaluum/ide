package se.scalablesolutions.akka.dispatch 
import java.util.concurrent.{LinkedBlockingQueue,ThreadFactory}
import java.util.Queue

import se.scalablesolutions.akka.actor.{Actor, ActorRef}
import org.zaluum.runtime.RealtimeThreadFactory
/**
 * Dedicates a unique thread for each actor passed in as reference. Served through its messageQueue.
 *
 * @author <a href="http://jonasboner.com">Jonas Bon&#233;r</a>
 */
/*class RealtimeThreadBasedDispatcher(val actor: ActorRef, val factory : ThreadFactory = RealtimeThreadFactory.default) extends MessageDispatcher {
  private val name = actor.getClass.getName + ":" + actor.uuid
  private val threadName = "rtthread-based:dispatcher:" + name
  private val queue = new BlockingMessageQueue(name)
  private var selectorThread: Thread = _
  @volatile private var active: Boolean = false

  def dispatch(invocation: MessageInvocation) = queue.append(invocation)

  def start = if (!active) {
    active = true
    selectorThread = factory.newThread(new Runnable{
      override def run = {
        while (active) {
          try {
            actor.invoke(queue.take)
          } catch { case e: InterruptedException => active = false }
        }
      }
    })
    selectorThread.setName(threadName)
    selectorThread.start
  }

  def isShutdown = !active

  def usesActorMailbox = false

  def shutdown = if (active) {
    log.debug("Shutting down RealtimeThreadBasedDispatcher [%s]", name)
    active = false
    selectorThread.interrupt
    references.clear
  }
}*/
