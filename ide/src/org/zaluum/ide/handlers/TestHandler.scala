package org.zaluum.ide.handlers

import org.eclipse.core.commands.AbstractHandler
import org.eclipse.core.commands.ExecutionEvent
import se.scalablesolutions.akka.remote._
import org.zaluum.runtime.Message
class TestHandler extends AbstractHandler {

  def execute(ev: ExecutionEvent): Object = { 
    val actor = RemoteClient.actorFor("zaluum-service", "localhost", 9999)
    val res = actor !! Message("hola")
    println(res)
    null
  }

}