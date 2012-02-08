package org.zaluum.launch;

import java.awt.Component;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.swing.JComponent;
import javax.swing.JFrame;

import org.zaluum.annotation.Apply;

public class Run {

	public static void main(String[] args) throws ClassNotFoundException,
			InstantiationException, IllegalAccessException, SecurityException,
			NoSuchMethodException, IllegalArgumentException,
			InvocationTargetException, NoSuchFieldException {
		Class<?> c = Thread.currentThread().getContextClassLoader()
				.loadClass(args[0]);
		final Object instance = c.newInstance();
		try {
			Method applyMethod = null;
			for (Method m : c.getMethods()) {
				if (m.getAnnotation(Apply.class) != null) {
					applyMethod = m;
					break;
				}
			}
			final Method runMethod = applyMethod == null ? c.getMethod("run")
					: applyMethod;
			Component comp = (Component) instance;
			JFrame frame = new JFrame();
			frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
			frame.add(comp);
			frame.pack();
			frame.setVisible(true);
			runMethod.invoke(instance);
		} catch (NoSuchFieldError e) {
			System.err.println("FATAL : " + args[0]
					+ " is not a widget Zaluum class");
			return;
		} catch (NoSuchMethodError e) {
			System.err.println("FATAL : " + args[0]
					+ " has not an apply() method");
		}

	}
}
