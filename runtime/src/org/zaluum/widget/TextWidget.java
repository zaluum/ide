package org.zaluum.widget;

import java.lang.reflect.InvocationTargetException;

import javax.swing.JTextField;

import org.zaluum.annotation.Apply;
import org.zaluum.annotation.Box;

@Box
public class TextWidget {
  public JTextField _widget = new JTextField();
  public TextWidget() {
	  _widget.setEnabled(false);
}
  @Apply
  public void apply(double d) throws InterruptedException, InvocationTargetException {
/*	  SwingUtilities.invokeAndWait(new Runnable() {

		@Override
		public void run() {*/
			_widget.setText(""+d);
/*		}
		  
	  });*/
  }
}
 