package org.zaluum.example;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class Run {

	/**
	 * @param args
	 * @throws ClassNotFoundException 
	 * @throws IllegalAccessException 
	 * @throws InstantiationException 
	 * @throws NoSuchMethodException 
	 * @throws SecurityException 
	 * @throws InvocationTargetException 
	 * @throws IllegalArgumentException 
	 */
	public static void main(String[] args) throws ClassNotFoundException, InstantiationException, IllegalAccessException, SecurityException, NoSuchMethodException, IllegalArgumentException, InvocationTargetException {
		Class<?> c = Class.forName("org.zaluum.example.TestModel");
		Object instance = c.newInstance();
		Method m = c.getMethod("apply");
		m.invoke(instance);
	}

}
