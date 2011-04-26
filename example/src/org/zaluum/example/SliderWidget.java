package org.zaluum.example;

import javax.swing.JSlider;

import org.zaluum.runtime.*;

@Box
public class SliderWidget {
	@Out(x=48,y=24) public double out = 0.0;
	public JSlider _widget = new JSlider();
	public SliderWidget() {
		_widget.setMaximum(800);
		_widget.setMinimum(-800);
		_widget.setValue(0);
	}
	void apply() {
		out=_widget.getValue();
	}
}
