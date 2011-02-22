package org.zaluum.example;

import javax.swing.JComponent;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.In;
import org.zaluum.nide.java.Widget;

@Box
public class TextWidget {
  @In(x=0,y=24)  
  public double d = 0.0;
  public JTextField _widget = new JTextField();
  public void apply() {
	  SwingUtilities.invokeLater(new Runnable() {

		@Override
		public void run() {
			_widget.setText(""+d);
		}
		  
	  });
  }
}
 