package org.zaluum.widget;

import java.lang.reflect.InvocationTargetException;

import javax.swing.JTextField;

import org.zaluum.annotation.Box;
import org.zaluum.annotation.In;


@Box
public class TextWidget {
  @In public boolean d ;
  public JTextField _widget = new JTextField();
  public TextWidget() {
	  _widget.setEnabled(false);
}
  public void apply() throws InterruptedException, InvocationTargetException {
/*	  SwingUtilities.invokeAndWait(new Runnable() {

		@Override
		public void run() {*/
			_widget.setText("diag: "+d);
/*		}
		  
	  });*/
  }
}
 