package org.zaluum.example;

import javax.swing.JComponent;
import javax.swing.JSlider;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.Out;
import org.zaluum.nide.java.Widget;

@Box
@Widget("javax.swing.JSlider")
public class SliderWidget {
	@Out(x=48,y=24) public double out = 0.0;
	public JComponent widget = new JSlider();
	void apply() {
		out=((JSlider)widget).getValue();
	}
}
