package org.zaluum.basic;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

public class RunScript {

	private static ScriptEngine engine;

	public static void run(String script, Object o) {
		if (engine == null) {
			ScriptEngineManager manager = new ScriptEngineManager();
			engine = manager.getEngineByName("JavaScript");
		}
		if (engine == null)
			throw new RuntimeException(
					"Cannot find Javascript Script Engine. Add rhino.jar to the classpath");
		try {
			engine.put("c", o);
			engine.eval(script);
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
}
