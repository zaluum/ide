package org.zaluum.example;

import java.lang.reflect.InvocationTargetException;

import javax.swing.JTextField;

import org.zaluum.runtime.Box;
import org.zaluum.runtime.In;

@Box
public class TextWidget {
  @In(x=0,y=24)  
  public boolean d ;
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
 