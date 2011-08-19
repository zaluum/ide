package org.zaluum.launch;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
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
    Class<?> c = Thread.currentThread().getContextClassLoader()
        .loadClass(args[0]);
    final Object instance = c.newInstance();
    try {
      Field f = c.getField("_widget");
      final Method m = c.getMethod("run");
      JComponent comp = (JComponent) f.get(instance);
      JFrame frame = new JFrame();
      frame.add(comp);
      frame.setSize(comp.getSize().width + 30, comp.getSize().height + 40);
      frame.addWindowListener(new WindowAdapter() {
        @Override
        public void windowClosing(final WindowEvent w) {
          System.exit(0);
        }
      });
      new Thread(new Runnable() {
        public void run() {
          try {
            m.invoke(instance);
          } catch (InvocationTargetException e) {
            e.getCause().printStackTrace();
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
      }).start();
      frame.setVisible(true);
    } catch (NoSuchFieldError e) {
      System.err
          .println("FATAL : " + args[0] + " is not a widget Zaluum class");
      return;
    } catch (NoSuchMethodError e) {
      System.err.println("FATAL : " + args[0] + " has not an apply() method");
    }

  }
}
