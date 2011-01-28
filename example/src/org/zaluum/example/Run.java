package org.zaluum.example;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.swing.JComponent;
import javax.swing.JFrame;

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
	 * @throws NoSuchFieldException
	 */
	public static void main(String[] args) throws ClassNotFoundException,
			InstantiationException, IllegalAccessException, SecurityException,
			NoSuchMethodException, IllegalArgumentException,
			InvocationTargetException, NoSuchFieldException {
		Class<?> c = Thread.currentThread().getContextClassLoader().loadClass(
				"org.zaluum.example.testInner");
		// Class<?> c =
		// Class.forName("org.zaluum.example.TestModel",true,Thread.currentThread().getContextClassLoader());
		final Object instance = c.newInstance();
		Field f = c.getField("widget");
		final Method m = c.getMethod("apply");
		JComponent comp = (JComponent) f.get(instance);
		JFrame frame = new JFrame();
		frame.add(comp);
		frame.setSize(comp.getSize());
		new Thread(new Runnable() {
			public void run() {
				try {
					while (true) {
						m.invoke(instance);
						Thread.sleep(100);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}).start();
		frame.setVisible(true);
	}
}
