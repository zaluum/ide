/*package se.scalablesolutions.akka.dispatch
import se.scalablesolutions.akka.actor.ActorRef
import org.eclipse.swt.widgets.Display
class ThreadBasedDispatcher(private val actor: ActorRef, private val display:Display) 
  extends MessageDispatcher {
  private val name = actor.getClass.getName + ":" + actor.uuid
  @volatile private var active: Boolean = false

  def dispatch(invocation: MessageInvocation) = display.asyncExec(new Runnable {
    override def run {
      try {
        actor.invoke(invocation)
      } catch { case e: InterruptedException => active = false }
    }
  })

  def start = if (!active) {
    active = true
  }

  def isShutdown = !active

  def usesActorMailbox = false
  def mailboxSize(actorRef: ActorRef):Int = 0
  def shutdown = if (active) {
    log.debug("Shutting down SWTThreadBasedDispatcher [%s]", name)
    active = false
    //references.clear
  }
}
*/