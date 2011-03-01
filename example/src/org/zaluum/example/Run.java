package org.zaluum.example;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.swing.JComponent;
import javax.swing.JFrame;

public class Run {

	public static void main(String[] args) throws ClassNotFoundException,
			InstantiationException, IllegalAccessException, SecurityException,
			NoSuchMethodException, IllegalArgumentException,
			InvocationTargetException, NoSuchFieldException {
		Class<?> c = Thread.currentThread().getContextClassLoader().loadClass(
				"org.zaluum.example.a");
		final Object instance = c.newInstance();
		Field f = c.getField("_widget");
		final Method m = c.getMethod("apply");
		JComponent comp = (JComponent) f.get(instance);
		JFrame frame = new JFrame();
		frame.add(comp);
		frame.setSize(comp.getSize());
		new Thread(new Runnable() {
			public void run() {
				try {
					m.invoke(instance);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}).start();
		frame.setVisible(true);
	}
}
