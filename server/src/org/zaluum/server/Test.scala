package org.zaluum.server
import se.scalablesolutions.akka.remote._
import org.zaluum.runtime._

class Test {
   val actor = RemoteClient.actorFor("zaluum-service", "localhost", 9999)
}