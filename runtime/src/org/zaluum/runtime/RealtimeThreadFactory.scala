package org.zaluum.runtime

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import scala.collection.JavaConversions._;
import scala.util.control.Exception._
object RealtimeThreadFactory {
	val customMethod= catching(classOf[NoSuchMethodException]) opt {
				classOf[Thread].getMethod("setRTPriority", classOf[Int]);
				}
	val realtimeConstructor = catching(classOf[ClassNotFoundException]) opt {
			val clazz = Thread.currentThread().getContextClassLoader()
					.loadClass("javax.realtime.RealtimeThread");
			clazz.getConstructors find { _.getParameterTypes.length == 6 }
	}
	val default = new RealtimeThreadFactory()
}
class RealtimeThreadFactory private (val back : ThreadFactory) extends ThreadFactory {
	
	def this() {
		this(Executors.defaultThreadFactory)
	}
	import RealtimeThreadFactory._
	def newThread(r:Runnable) : Thread = {
  		if (customMethod.isDefined) {
  			val newThread = back.newThread(r);
  			customMethod.get.invoke(newThread, 1.asInstanceOf[java.lang.Integer]);
  			println("New custom OpenJDK thread" + newThread);
  			return newThread 				
  		} else if (realtimeConstructor.isDefined) {
  			/*
  			 * public RealtimeThread(SchedulingParameters scheduling,
  			 * ReleaseParameters release, MemoryParameters memory,
  			 * MemoryArea area, ProcessingGroupParameters group,
  			 * java.lang.Runnable logic)
  			 */
  			val newThread = realtimeConstructor.get.get.newInstance(null,
  					null, null, null, null, r).asInstanceOf[Thread];
  			println("New RTSJ thread " + newThread)
  			return newThread
  		} else {
  			val newThread = back.newThread(r)
  			newThread.setPriority(Thread.MAX_PRIORITY)
  			println("New normal rt thread " + newThread)
  			return newThread
  		}
	}

}
/**
 * Copyright (C) 2009-2010 Scalable Solutions AB <http://scalablesolutions.se>
 */

