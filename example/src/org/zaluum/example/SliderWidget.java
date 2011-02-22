package org.zaluum.example;

import javax.swing.JSlider;

import org.zaluum.nide.java.Box;
import org.zaluum.nide.java.Out;

@Box
public class SliderWidget {
	@Out(x=48,y=24) public double out = 0.0;
	public JSlider _widget = new JSlider();
	void apply() {
		out=_widget.getValue();
	}
}
